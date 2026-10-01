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

(require 'treesit)
(require 'project)

(defun zerolee--markdown-get-target ()
  "从当前节点开始往外查找，直到找到指定的符号."
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (parent)
     (member (treesit-node-type parent)
             '("link_reference_definition" "shortcut_link" "full_reference_link" "inline_link")))))

(defun zerolee--markdown-goto-def (NODE)
  "去寻找当前 NODE 所引用的定义的位置"
  (when-let*
      ((def
        (save-excursion
          (search-forward
           (concat (treesit-node-text NODE t) ":") nil t))))
    (goto-char def)))

(defun zerolee--markdown-open-wiki (wiki)
  "打开 wiki 标记"
  ;; [[wiki]]，[[wiki|look]],[[wiki#chapter]],[wiki.pdf]
  ;; 只要 | 前面的部分, 只有 markdown 文件的#后面有用
  (let* ((truename (car (string-split (treesit-node-text (treesit-node-child wiki 1) t) "|")))
         (filenames (string-split truename "#"))
         (filename (car filenames)))
    (if (string-search "." filename)    ;是否是 pdf 之类的文件
        (if-let* ((pc (project-current))
                  (fn (directory-files-recursively (project-root (project-current)) filename)))
            (browse-url (car fn))
          (when (file-exists-p filename)
            (browse-url filename)))
      (let* ((file (concat filename ".md")) ;markdown 文件
             (path (expand-file-name file (file-name-directory (buffer-file-name)))))
        (if-let* ((pc (project-current))
                  (fn (directory-files-recursively (project-root (project-current)) file)))
            (progn
              (find-file (car fn))
              (when-let* ((chapter (cl-second filenames)))
                (progn
                  (goto-char (point-min))
                  (when (search-forward chapter nil t)
                    (beginning-of-line)))))
          (if (file-exists-p path)
              (progn
                (find-file path)
                (when-let* ((chapter (cl-second filenames)))
                  (progn
                    (goto-char (point-min))
                    (when (search-forward chapter nil t)
                      (beginning-of-line)))))
            (when (y-or-n-p (format "File %s does not exist. Create? " path))
              (find-file path))))))))

(defun zerolee-markdown-do ()
  "markdown-do在 treesit下的实现"
  (interactive)
  (let* ((target (zerolee--markdown-get-target))
         (type (treesit-node-type target)))
    (cond
     ((string= type "link_reference_definition")
      "这是脚注的定义")
     ((string= type "full_reference_link")
      ;; [][] -> []: something
      (zerolee--markdown-goto-def (treesit-node-child target 3)))
     ((string= type "inline_link")
      ;; [百度](https://baidu.com)
      (browse-url (treesit-node-text (treesit-node-child target 4) t)))
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
