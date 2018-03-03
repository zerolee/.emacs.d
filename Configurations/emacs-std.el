(message "Loading emacs-std...")
(display-time)
;; 关掉开机信息
(setq inhibit-startup-message t)

;; 使用主题
(load-theme 'adwaita t)

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


;; display the number of column
(setq column-number-mode t)

;;;Always do syntax highligting
(global-font-lock-mode 1)

;;;Also highlight parens,
(show-paren-mode t)
;;showing matching parentheses
;;(setq show-paren-delay 0
(setq show-paren-style 'parenthesis)
;; showing region between parentheses
;;(setq show-paren-style 'expression)

;;;if you want to remove menu,you can insert the following line
(menu-bar-mode 0)
;;;if you want to remove tool...you can
(tool-bar-mode 0)
;;
(scroll-bar-mode 0)

;;;edit Compressed File
(require 'jka-compr)


;;;display and open image
(auto-image-file-mode)

;;;arrest auto-save by session
;;;(setq auto-save-list-prefix nill)
(put 'upcase-region 'disabled 0)
(put 'downcase-region 'disabled 0)


;; 设置环境变量
(setenv "EMACS_START" "emacs_start")
 
;; Using MELPA
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))


(message "Loading emacs-std...done")
