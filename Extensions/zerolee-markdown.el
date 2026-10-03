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

;;; Code:


;;; 只有标数字的需要跳转，其他的就是 wiki，跟链接跳转(应该也是不需要)
;;; wiki link其实就是去跳转文件，带 # 还可以加上行数
;;; [hello][world]
;; [[wikitarget|text]]
;; ^[鲁迅]
;; 脚注
;; 1. 引用 [^1]即[link_text] ，注释 [^1]: 脚注注释
;; 2. 引用 ^[鲁迅]即 ^shortcut_link[link_text],
;; wiki link
;; 1. [[wiki链接]], 即 [shortcut_link[wiki 链接]]
;; 2. [[wiki链接|别名]]
;; 3. [[wiki链接#标题]]
;; 4. [[wiki链接#^段落]]
;; 脚注+wiki
;; 1. ^[[[wiki链接]]]
;; 其他
;; [baidu](https://baidu.com)
;; [link_text]shortcut_link link_label link_text
;; ![图片](图片.jpg)
;;(inline
;; (image ! [ (image_description) ] (
;;  (link_destination .)
;;  (link_title " ")
;;  )))
;;  这是把上面两个结合起来了
;;  [![图片](图片.jpg)](https://baidu.com)
;; (inline
;;  (inline_link [
;;   (link_text
;;    (image ! [ (image_description) ] (
;;     (link_destination : / / . / .)
;;     )))
;;   ] (
;;   (link_destination : / / .)
;;   (link_title " ")
;;   )))


;; 看起来花样繁多实际上对于 treesitter 而言主要分为两种，即
;; 1. 脚注定义,即 [^1]: 脚注定义。 在tree sitter眼中它是
;;(link_reference_definition
;; (link_label [ ^ ])
;; : (link_destination))
;; 2. 被中括号括起来的存在，中括号括起来后他叫 shortcut_link
;;    中括号中的内容叫作 link_text
;;    [link_text],
;;    ^[link_text],
;;    ^[[shortcut_link[]]]
;;我往外寻找，是 link_reference_definition 或 shortcut_link 即停
;;link_reference_definition 找到第二个子节点然后搜索引用 xref
;;shortcut_link 则是分情况讨论
;;(full_reference_link [ (link_text) ]
;; (link_label [ ]))

(require 'cl-lib)
(require 'treesit)
(require 'project)

(defconst zerolee--markdown-link-node-types
  '("link_reference_definition"
    "shortcut_link"
    "full_reference_link"
    "inline_link"
    "image")
  "会被 `zerolee-markdown-do' 识别的 tree-sitter 节点类型.")

(defun zerolee--markdown-get-link-node ()
  "从当前节点开始往外查找，直到找到指定的符号."
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (parent)
     (member (treesit-node-type parent)
             zerolee--markdown-link-node-types))))

(defun zerolee--markdown-goto-def (node)
  "在当前缓冲区中查找 NODE 所引用的脚注定义并跳转到其行首."
  (when (search-forward
         (concat (treesit-node-text node t) ":") nil t)
    (goto-char (match-beginning 0))))

(defun zerolee--markdown-find-file (filename)
  "查找需要的文件，
先在当前目录下查找，若没有看是否存在项目，存在则在项目下查找"
  (if (file-exists-p filename)
      filename
    (when-let* ((pc (project-current))
                (fn (directory-files-recursively
                     (project-root pc) (regexp-quote filename))))
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
  (let* ((raw (treesit-node-text (treesit-node-child wiki 1) t))
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
      "这是脚注的定义")
     ((string= type "full_reference_link")      ;; [][] -> []: something
      (zerolee--markdown-goto-def (treesit-node-child target 3)))
     ((string= type "inline_link")     ;; [百度](https://baidu.com)
      (browse-url (treesit-node-text (treesit-node-child target 4) t)))
     ((string= type "image");; ![图片](图片.jpg)
      ;; [![图片](图片.jpg)](https://baidu.com)
      (if (string= "link_text" (treesit-node-type (treesit-node-parent target)))
          (browse-url (treesit-node-text
                       (treesit-node-child
                        (treesit-node-parent (treesit-node-parent target))
                        4)
                       t))
        ;; ![图片](图片.jpg)
        (browse-url (treesit-node-text (treesit-node-child target 5) t))))
     ((string= type "shortcut_link")
      ;; [^1], ^[1], [[]], ^[[[]]]
      (let* ((start (treesit-node-start target))
             (before (char-before start)))
        (cond ((char-equal ?\[ before) (zerolee--markdown-open-wiki target))
              ((char-equal ?^ before)
               ;; ^[footnote]
               "另种脚注")
              ((char-equal ?^ (char-after (+ start 1)))
               ;;  [^1] -> [^1]:something
               (zerolee--markdown-goto-def target))))))))

(provide 'zerolee-markdown)
;;; zerolee-markdown ends here
