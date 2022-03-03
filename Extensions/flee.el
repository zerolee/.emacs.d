;;;  flee.el ---  逃离字符串和括号 -*- lexical-binding: t; -*-

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

;;; 在书写 Lisp 的时候，总有一些想要跳出的地方，比如 docing, 比如
;;; let 的 bind 的部分，比如 cond 的某一个子表达式……
;;; Code:

(require 'tree-sitter)
(require 'vesie)
(require 'paredit)

(defconst flee-bind-list
  '("let" "let*" "symbol-macrolet" "symbol-macrolet*" "cl-symbol-macrolet"
    "cl-symbol-macrolet*" "if-let" "if-let*" "when-let" "when-let*")
  "保存类似 let 那样的结构，重心在 binds 的列表.")

(defconst flee-clause-list '("case" "ecase" "cl-ecase" "cl-case" "cond"
			     "eval-when" "cl-eval-when")
  "保存类似 cond 那样的结构，重心在 clause 的列表.")

(defconst flee-def-list '("defun" "cl-defun" "defmacro" "cl-defmacro")
  "保存类似定义宏，定义函数那样的结构.")

(defconst flee-target-list  (append flee-bind-list flee-clause-list flee-def-list)
  "保存想要查找的目标的列表.")

(defun flee-get-target ()
  "从当前节点开始往外查找，直到找到指定的符号."
  (tree-sitter-parent-until
   (tree-sitter-node-at (point))
   (lambda (parent)
     (member (tree-sitter-node-text (tree-sitter-node-child parent 1) t)
	     flee-target-list))))

(defun flee-child-end (parent)
  "去 `parent' 在 (point) 处的子节点的末尾."
  (goto-char
   (tree-sitter-node-end
    (tree-sitter-node-first-child-for-pos parent (point)))))

;;;###autoload
(defun flee-dwim ()
  "退出 case, ecase… 的单个 (KEYLIST BODY...) 部分，
let, let*, symbol-macrolet… 的单个 bind 部分."
  (interactive)
  (setq-local tree-sitter-parser-list nil)
  (tree-sitter-get-parser-create 'tree-sitter-elisp)
  (if-let ((target (flee-get-target))
	   (sibling1 (tree-sitter-node-text
		      (tree-sitter-node-child target 1) t)))
      (if (and (member sibling1 flee-bind-list)
	       (< (point) (tree-sitter-node-end
			   (tree-sitter-node-child target 2))))
	  (flee-child-end (tree-sitter-node-child target 2))
	(if (or (member sibling1 flee-clause-list)
		(and (member sibling1 flee-def-list)
		     (tree-sitter-node-eq (tree-sitter-node-parent
					   (tree-sitter-node-at (point)))
					  target)))
	    (flee-child-end target)
	  (search-forward ")" (point-at-eol) t 1)))
    (search-forward ")" (point-at-eol) t 1))
  (paredit-newline)
  (vesie-mode 0))


(provide 'flee)
;;; flee.el ends here
