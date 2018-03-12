(display-time)
;; 关掉开机信息
(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq column-number-mode t)


;; hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-line
	try-expand-list
	try-expand-all-abbrevs
	try-expand-dabbrev-all-buffers
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
	try-expand-dabbrev-from-kill))


;; 在重名buffer前面加上其父目录的名字
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;Also highlight parens,
(show-paren-mode t)
;;showing matching parentheses
;;(setq show-paren-delay 0
(setq show-paren-style 'parenthesis)
;; showing region between parentheses
;;(setq show-paren-style 'expression)


;;;edit Compressed File
(require 'jka-compr)


;;;display and open image
(auto-image-file-mode)


(put 'upcase-region 'disabled 0)
(put 'downcase-region 'disabled 0)


(delete-selection-mode t)

;;; org
(setq org-agenda-files '("~/note/plan"))


;; 设置环境变量
(setenv "EMACS_START" "emacs_start")

;;; .cquery 导入
(add-hook 'before-save-hook
	  '(lambda ()
	     (if (string-equal (file-name-nondirectory (buffer-file-name)) ".cquery")
		 (unless (file-exists-p (buffer-file-name))
		     (insert-file-contents "~/模板/.cquery")))))
 
;; Using MELPA
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
