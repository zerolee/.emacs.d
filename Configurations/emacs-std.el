(message "Loading emacs-std...")
(display-time)
;; 关掉开机信息
(setq inhibit-startup-message t)

;; 使用主题
(load-theme 'adwaita t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; open semantic-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;(semantic-mode t)
;;;;;;;;; add two auxiliary global minor modes
;;;;;(global-semantic-idle-summary-mode)
;;;;;;;(global-semantic-idle-completions-mode)
;;;;;;; use semantic in speedbar
;;;;;(require 'semantic/sb)


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

;; Cscope
;;;;;(add-hook 'c-mode-common-hook
;;;;;	  '(lambda ()
;;;;;	     (require 'xcscope)))


;; Global
;;(autoload 'gtags-mode "gtags" "" t)
;;(add-hook 'c-mode-common-hook
;;	  '(lambda()
;;	     (gtags-mode 1)))
;;(add-hook 'asm-mode-hook
;;	  '(lambda()
;;	     (gtags-mode 1)))
;; 在重名buffer前面加上其父目录的名字

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; addition the directory of info
;(setq Info-default-directory-list (append
;				   Info-default-directory-list
;				   '("~/backup/src/emacs-24.4/info")))


;; if you want Emacs to always match using case(such as, Foo shoud
;; not be matched by foo), insert the following line:
;; (setq-default case-fold-search nil)

;; display the number of column
(setq column-number-mode t)

;; Saving the Buffer List
;; it might be desirable to be able to make a dump of Emacs(by type
;; "destop-save"), this dump should contain enough insormation to
;;make Emacs configure itself to the state it bad before you exited it.
;; (load "desktop")
;; (desktop-load-default)
;; (desktop-read)


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

;;;make backup in one directory
;;;(require 'backup-dir)
;;;(setq bkup-backup-directory-info
;;;      '((t "/home/zmqc/.backups" ok-create full-path)))
;;;
;;;arrest auto-save by session
;;;(setq auto-save-list-prefix nill)
(put 'upcase-region 'disabled 0)
(put 'downcase-region 'disabled 0)

;; Org-mode
;; :base-directory - 你存放笔记的目录（想将哪里的org文件导出成HTML）
;; :base-extension - 导出的文件格式
;; :publishing-directory - 导出HTML的目标目录
;; :recursive - 设置为t会将子目录中的文件也导出
;; :publishing-function - 使用哪个函数来进行publish（注：org 7与8在这个地方有区别）
;; :auto-sitemap - 自动生存sitemap
;; :sitemap-sort-files - 我这里采用的是按照从新到旧的排列方式
;; :sitemap-file-entry-format - 这里采用时间+标题的方式生成sitemap
;; M-x org-publish-project，输入blog，就会自动开始生成HTML文件

(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done 'time)

(require 'ox-publish)
(setq org-publish-project-alist
      '(("blog-notes"
	 :base-directory "~/note"
	 :base-extension "org"
	 :publishing-directory "~/tmp/publicnote"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4
	 :auto-preamble t
	 :section-numbers nil
  	 :author "zmqc"
  	 :email "zmqclee@gmail.com"
  	 :auto-sitemap t              ;Generate sitemap.org automagically..
  	 :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
  	 :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
  	 :sitemap-sort-files anti-chronologically
  	 :sitemap-file-entry-format "%d %t"
  	 :table-of-contents t
  	 :html-head "<link rel='stylesheet' type='text/css' href='style/style.css'/>"
	 )
	("blog-static"
	 :base-directory "~/note"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/tmp/publicnote"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("blog":components ("blog-notes" "blog-static"))
      ))

;; 设置环境变量
(setenv "EMACS_START" "emacs_start")
 
;; Using MELPA
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))


(message "Loading emacs-std...done")
