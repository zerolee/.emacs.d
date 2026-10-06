;;; zerolee-markdown.el ---  markdown 相关 -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 基于 tree-sitter 的 `markdown-do' 实现：
;;   支持的 tree-sitter 节点类型：
;;   1. link_reference_definition：: 即 [^1]: 脚注定义
;;   2. shortcut_link：: [^1], ^[1], [[wiki]], [[wiki|别名]],
;;                       [[wiki#标题]],[[wiki#^段落]],^[[[wiki]]]，[[#anchor]]
;;   3. full_reference_link：[text][label]
;;   4. inline_link：[百度](https://baidu.com)，[![alt](src)](url)
;;   5. collapsed_reference_link: [label][] -> [label]: something2
;;   6. image：![alt](src)

;;   7. 其他 <a id="custom-id"></a>,<span id="custom-id"></span>, {#id}

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'project)
(require 'markdown-ts-mode)

(defvar-local zerolee-markdown--origin-pos nil
  "保存跳转前的位置，供 `zerolee-markdown-do' 带前缀参数时回跳。")

(defconst zerolee-markdown--link-node-types
  '("link_reference_definition"         ;[]: something
    "shortcut_link" ;; [^1], ^[1], [[wiki]], ^[[[wiki]]],[[#anchor]]
    "full_reference_link"      ;; [text][label] ->  [label]: something
    "collapsed_reference_link" ;[label][] -> [label]: something2-
    "inline_link"              ;; [百度](https://baidu.com)
    "image") ;; ![图片](图片.jpg), [![图片](图片.jpg)](https://baidu.com)
  "会被 `zerolee-markdown-do' 识别的 tree-sitter 节点类型.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 基础工具
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zerolee-markdown--child-by-type (node type)
  "返回 NODE 中类型为 TYPE 的第一个直接子节点，找不到返回 nil."
  (car (treesit-filter-child
        node
        (lambda (child)
          (string= (treesit-node-type child) type)))))

(defun zerolee-markdown--child-text (node type)
  "返回 NODE 中类型为 TYPE 的子节点文本，找不到返回 nil。"
  (treesit-node-text (zerolee-markdown--child-by-type node type) t))

(defun zerolee-markdown--get-link-node ()
  "从当前节点开始往外查找，直到找到指定的符号."
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (parent)
     (member (treesit-node-type parent)
             zerolee-markdown--link-node-types))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 定位/查找
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zerolee-markdown--goto-def (label)
  "在当前缓冲区查找 LABEL 对应的定义并跳转到其行首。
LABEL 例如 \"[^1]\" 或 \"[label]\"。找不到时给出提示并保持在原位。"
  (let ((origin (point)))
    (goto-char (point-min))
    (unless
        (catch 'found
          (while (search-forward (concat label ":") nil t)
            (unless (markdown-ts-at-code-block-p)
              (setq origin (match-beginning 0))
              (throw 'found t))))
      (message "%s未定义" label))
    (goto-char origin)))

(defun zerolee-markdown--goto-ref (label)
  "在当前缓冲区中查找所有 NODE 的引用,
如果有一个话直接跳转，否则让用户自己选择."
  (let ((origin (point))
        (refs '()))
    (goto-char (point-min))
    (while (search-forward label nil t)  ;搜集所有引用
      (unless (or (markdown-ts-at-code-block-p) ;代码块里的不要
                  (eq (char-after) ?:)) ;定义地方的不要
        (push
         (cons
          (format "%d: %s"
                  (line-number-at-pos)
                  (buffer-substring (pos-bol) (point)))
          (match-beginning 0))
         refs)))
    (goto-char origin)                  ;恢复
    (when refs
      (if (eql 1 (length refs))
          (setq origin (cdar refs))     ;只有一个直接跳转
        (let ((choose (completing-read "refs: " (reverse refs)))) ;用户的选择
          (setq origin (cdr (assoc choose refs))))))
    (goto-char origin)                  ;跳转
    (unless refs                        ;一个没有，给出提示
      (message "不存在 %s 的引用" label))))

(defun zerolee-markdown--find-file (filename)
  "查找需要的文件，
先在当前目录下查找，若没有看是否存在项目，存在则在项目下查找,
在项目下递归查找时跳过 .git 等隐藏目录."
  (if (file-exists-p filename)
      filename
    (when-let* ((project (project-current))
                (fn (directory-files-recursively
                     (project-root project)
                     (concat "\\`" (regexp-quote filename) "\\'")
                     nil
                     (lambda (dirname)
                       (not (string-match-p "/\\.[^/]+\\'" dirname))))))
      (car fn))))

(cl-defun zerolee-markdown--goto-anchor (anchor &aux (origin (point)))
  "在当前缓冲区跳转到 ANCHOR 指定的位置。
支持 标题（# 标题, #标题{#id}） 、 段落块 ID（#^blockid）,
自定义<a id=\"custom-id\"></a>三种形式。"
  (when anchor
    (goto-char (point-min))
    ;; ^[blockid]:直接搜索 anchor
    (if (string-prefix-p "^" anchor)
        (if (search-forward anchor nil t)
            (beginning-of-line)
          (goto-char origin))
      ;; 反之, 调用函数查询所有生成的 id可以完成标题以及{#anchor}的跳转
      (if-let* ((pos (gethash (markdown-ts--slug-github anchor) (markdown-ts--heading-ids))))
          (goto-char pos)
        ;; 寻找<a/span id="id"></a>
        (if (search-forward-regexp
             (format "<\\(a\\|span\\)\\s-+id=\"%s\"" (regexp-quote anchor)) nil t)
            (beginning-of-line)
          (goto-char origin))))))

(defun zerolee-markdown--open-wiki (wiki)
  "打开 wiki 标记"
  ;; [[wiki]]，[[wiki|look]],[[wiki#chapter]],[wiki.pdf],[#anchor]
  ;; 只要 | 前面的部分, 只有 markdown 文件的#后面有用
  (let* ((target (car (string-split wiki "|")))
         (parts (string-split target "#"))
         (filename (car parts)))
    (if (string-search "." filename) ;非 markdown 文件(pdf 等)，交由默认程序打开
        (when-let* ((f (zerolee-markdown--find-file filename)))
          (browse-url (expand-file-name f (or (file-name-directory (buffer-file-name))
                                              default-directory))))
      (if (string-empty-p filename)     ;[[#anchor]]直接跳转当前文件的anchor
          (zerolee-markdown--goto-anchor (cadr parts))
        (let* ((file (concat filename ".md"))) ;markdown 文件
          (if-let* ((f (zerolee-markdown--find-file file)))
              (progn
                (find-file f)
                (zerolee-markdown--goto-anchor (cadr parts)))
            (when (y-or-n-p (format "File %s does not exist. Create? " file))
              (find-file file))))))))

(defun zerolee-markdown-do (&optional arg)
  "markdown-do在 treesit下的实现, 加参数跳回来的位置."
  (interactive "P")
  (if arg
      (goto-char zerolee-markdown--origin-pos)
    (let* ((target (zerolee-markdown--get-link-node))
           (type (treesit-node-type target)))
      (setq-local zerolee-markdown--origin-pos (point))
      (cond
       ((string= type "link_reference_definition") ;[^1]: something -> [^1]
        (zerolee-markdown--goto-ref
         (zerolee-markdown--child-text target "link_label")))
       ((string= type "full_reference_link") ;; [text][label] ->  [label]: something
        (zerolee-markdown--goto-def
         (zerolee-markdown--child-text target "link_label")))
       ((string= type "collapsed_reference_link") ;[label][] -> [label]: something
        (zerolee-markdown--goto-def
         (concat "[" (zerolee-markdown--child-text target "link_text") "]")))
       ;;  [![图片](图片.jpg)](https://baidu.com), ![图片](图片.jpg), [百度](https://baidu.com)
       ((member type '("image" "inline_link"))
        (let ((parent-node (treesit-node-parent target)))
          (when (string= "link_text" (treesit-node-type parent-node))
            (setq target (treesit-node-parent parent-node)))
          (let ((text (zerolee-markdown--child-text target "link_destination")))
            (if (string-prefix-p "#" text)
                (if (length= text 1)
                    (goto-char (point-min)) ;[回到顶部](#)
                  (zerolee-markdown--goto-anchor (substring text 1))) ;; [跳到标题](#标题)
              (browse-url text)))))     ;打开链接
       ((string= type "shortcut_link")
        ;; [^1], ^[1], [[]], ^[[[]]], [[#custom-id]]
        (let* ((start (treesit-node-start target))
               (before (char-before start)))
          (cond ((eq ?\[ before)
                 (zerolee-markdown--open-wiki
                  (zerolee-markdown--child-text target "link_text")))
                ((eq ?^ before)
                 ;; ^[footnote]
                 (message "自定义的脚注"))
                ((eq ?^ (char-after (1+ start)))
                 ;;  [^1] -> [^1]:something
                 (zerolee-markdown--goto-def (treesit-node-text target t)))
                (t (message "未知的快捷链接形式")))))))))

(provide 'zerolee-markdown)
;;; zerolee-markdown.el ends here
