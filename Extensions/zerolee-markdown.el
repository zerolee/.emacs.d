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
;;   种类很多，但是对于 tree-sitter 而言只有 5 类
;;   1. link_reference_definition：脚注定义，即 [^1]: 脚注定义
;;   2. shortcut_link：[^1], ^[1],
;;                     [[wiki]], [[wiki|别名]],[[wiki#标题]],[[wiki#^段落]],^[[[wiki]]]
;;                     打开对应文件并跳转锚点，文件不存在时可创建。
;;   3. full_reference_link：[text][label]
;;   4. inline_link：[百度](https://baidu.com)，[![alt](src)](url)
;;   5. image：![alt](src)

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'project)
(require 'markdown-ts-mode)

(defconst zerolee--markdown-link-node-types
  '("link_reference_definition"
    "shortcut_link"
    "full_reference_link"
    "inline_link"
    "image")
  "会被 `zerolee-markdown-do' 识别的 tree-sitter 节点类型.")

(defun zerolee--markdown-child-by-type (node type)
  "返回 NODE 中类型为 TYPE 的第一个直接子节点，找不到返回 nil."
  (car (treesit-filter-child
        node
        (lambda (child)
          (string= (treesit-node-type child) type))
        t)))

(defun zerolee--markdown-get-link-node ()
  "从当前节点开始往外查找，直到找到指定的符号."
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (parent)
     (member (treesit-node-type parent)
             zerolee--markdown-link-node-types))))

(defun zerolee--markdown-goto-def (node)
  "在当前缓冲区中查找 NODE 所引用的脚注定义并跳转到其行首."
  (let* ((pos (point))
         (text (treesit-node-text node t)))
    (goto-char (point-min))
    (catch 'done
      (while (search-forward (concat text ":") nil t)
        (unless (markdown-ts-at-code-block-p)
          (setq pos (match-beginning 0))
          (throw 'done 0))))
    (goto-char pos)
    (message "%s未定义" text)))

(defun zerolee--markdown-find-file (filename)
  "查找需要的文件，
先在当前目录下查找，若没有看是否存在项目，存在则在项目下查找"
  (if (file-exists-p filename)
      filename
    (when-let* ((pc (project-current))
                (fn (directory-files-recursively
                     (project-root pc)
                     (concat "\\`" (regexp-quote filename) "\\'"))))
      (car fn))))

(defun zerolee--markdown-goto-anchor (anchor)
  "在当前缓冲区跳转到 ANCHOR 指定的位置。
支持 标题（#标题） 与 段落块 ID（#^blockid）两种形式。"
  (when anchor
    (goto-char (point-min))
    ;; ^[blockid]:直接搜索 anchor，反之在前面加#, 即#anchor
    (when (search-forward
           (concat
            (if (string-prefix-p "^" anchor) "" "# ")
            anchor)
           nil t)
      (beginning-of-line))))

(defun zerolee--markdown-open-wiki (wiki)
  "打开 wiki 标记"
  ;; [[wiki]]，[[wiki|look]],[[wiki#chapter]],[wiki.pdf]
  ;; 只要 | 前面的部分, 只有 markdown 文件的#后面有用
  (let* ((raw (treesit-node-text wiki t))
         (target (car (string-split raw "|")))
         (parts (string-split target "#"))
         (filename (car parts)))
    (if (string-search "." filename) ;非 markdown 文件(pdf 等)，交由默认程序打开
        (when-let* ((f (zerolee--markdown-find-file filename)))
          (browse-url (expand-file-name f (file-name-directory (buffer-file-name)))))
      (let* ((file (concat filename ".md"))) ;markdown 文件
        (if-let* ((f (zerolee--markdown-find-file file)))
            (progn
              (find-file f)
              (zerolee--markdown-goto-anchor (cadr parts)))
          (when (y-or-n-p (format "File %s does not exist. Create? " file))
            (find-file file)))))))

(defun zerolee-markdown-do ()
  "markdown-do在 treesit下的实现"
  (interactive)
  (let* ((target (zerolee--markdown-get-link-node))
         (type (treesit-node-type target)))
    (cond
     ((string= type "link_reference_definition")  ;[]: something
      (message "Footnote definition: %s"
               (treesit-node-text target t)))
     ((string= type "full_reference_link")      ;; [][] -> []: something
      (zerolee--markdown-goto-def
       (zerolee--markdown-child-by-type target "link_label")))
     ((string= type "inline_link")     ;; [百度](https://baidu.com)
      (browse-url
       (treesit-node-text
        (zerolee--markdown-child-by-type target "link_destination") t)))
     ((string= type "image");;  [![图片](图片.jpg)](https://baidu.com), ![图片](图片.jpg)
      (let ((parent-node (treesit-node-parent target)))
        (when (string= "link_text" (treesit-node-type parent-node))
          (setq target (treesit-node-parent parent-node)))
        (browse-url
           (treesit-node-text
            (zerolee--markdown-child-by-type target "link_destination") t))))
     ((string= type "shortcut_link")
      ;; [^1], ^[1], [[]], ^[[[]]]
      (let* ((start (treesit-node-start target))
             (before (char-before start)))
        (cond ((char-equal ?\[ before)
               (zerolee--markdown-open-wiki
                (zerolee--markdown-child-by-type target "link_text")))
              ((char-equal ?^ before)
               ;; ^[footnote]
               (message "另种脚注"))
              ((char-equal ?^ (char-after (+ start 1)))
               ;;  [^1] -> [^1]:something
               (zerolee--markdown-goto-def target))))))))

(provide 'zerolee-markdown)
;;; zerolee-markdown ends here
