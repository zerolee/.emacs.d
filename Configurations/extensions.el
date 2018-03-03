(message "Loading extensions...")
(require 'sams-lib)
;;(autoload 'sams-lib "sams-lib" nil t)

;;;template.el
;;;At begin, you need create the directory ~/lib/tempaltes, and copy
;;;some templates to there.
;;;(require 'template) 
;;;(template-initialize) 
;;;(setq template-home-directory "/")

;;;加入到 .emacs 文件。使用 M-x color-theme-select 就会出现一个配色方案选择窗口，
;;;在配色方案上按 l 就可以改变当前 frame 的配色，按 i 就可;以改变 所有 frame 的配色。
;;;如果你想选定一个配色方案后就一直用它，而避免每次都加载大量用不着的 lisp代码，
;;;按 p 就可以把当前配色方案的 lisp 打印出来，你可以把它加到你 的 .emacs 文件。而不使用 (require 'color-theme) 这样可以加快启动速度。
;;;(if (equal window-system 'x)
;;;    (progn
;;;      (require 'color-theme)
;;;      (color-theme-gnome2)))

;;(if (equal window-system 'x)
;;(load-theme 'solarized-dark t))




;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; ecb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'ecb)
;; 
;; (setq ;ecb-tip-of-the-day nil
;;  ecb-tree-indent 4
;; ; ecb-windows-height 0.5
;; ; ecb-windows-width 0.25
;;  ecb-auto-compatibility-check 0
;;  ecb-version-check 0)
;; (global-set-key (kbd "C-c <left>") 'windmove-left)
;; (global-set-key (kbd "C-c <right>") 'windmove-right)



;;;;;;;;;;;;;;;;;;;;;
;;;;; helm
;;;;;;;;;;;;;;;;;;;;;
;;;(require 'helm-config)
;;;(helm-mode 1)
;;;(global-set-key (kbd "M-x") 'helm-M-x)
;;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;;(global-set-key (kbd "C-x r i") 'helm-register)

;;;;;;;;;;;;;;;;;;
;; helm-company
;;;;;;;;;;;;;;;;;;

;;;;;(autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
;;;(eval-after-load 'company
;;;  '(progn
;;;     (define-key company-mode-map (kbd "C-:") 'helm-company)
;;;     (define-key company-active-map (kbd "C-:") 'helm-company)))


;;;;;;;;;;;;;;;;;;
;; emacs-async
;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
;; ivy
;;;;;;;;;;;;;;;;;;;;
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)	; 将最近的文件和书签加入到 ivy-switch-buffer
(global-set-key (kbd "C-h a") 'counsel-apropos)
(global-set-key (kbd "M-y")   'counsel-yank-pop)
;;(global-set-key (kbd "M-w")   'ivy-kill-ring-save)

(global-set-key (kbd "C-x c l") 'counsel-locate)
(global-set-key (kbd "C-x c a") 'counsel-ag)
(global-set-key (kbd "C-x c f") 'counsel-fzf)
(global-set-key (kbd "C-x c i") 'counsel-imenu)

(global-set-key (kbd "C-x c p") 'ivy-push-view)

;; C-c C-o ivy-occur, C-x C-q 开始编辑, C-x C-s 保存编辑， C-c C-c 退出
(global-set-key (kbd "C-M-s") 'swiper)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key ivy-minibuffer-map (kbd "C-l") 'counsel-up-directory)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'global-company-mode)


;;(setq company-backends '(company-bbdb company-nxml company-css company-eclim company-semantic company-cmake company-capf company-files  (company-dabbrev-code company-keywords) company-oddmuse company-dabbrev company-yasnippet))

(setq company-backends '(company-bbdb company-nxml company-css company-eclim company-semantic company-cmake company-capf company-files  (company-dabbrev-code company-keywords) company-oddmuse company-dabbrev (company-yasnippet company-lsp)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cquery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xref-find-definitions ( M-. )
;; xref-find-references  ( M-? )
;; xref-find-apropos     ( C-M-. )
(setq cquery-executable "/home/zmqc/backups/src/cquery/cquery/build/release/bin/cquery")

(add-hook 'c-mode-hook
	  '(lambda ()
	     (require 'cquery)
	     (lsp-cquery-enable)
     	     (require 'company-lsp)
	     (push 'company-lsp company-backends)
     	     (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
	     (setq cquery--get-init-params '(:completion (:detailedLabel t)))
	     (require 'ivy-xref)
	     (setq xref-show-xrefs-function 'ivy-xref-show-xrefs)
	     (setq cquery-sem-highlight-method 'overlay)
	     (setq cquery-sem-highlight-method 'font-lock)))

(require 'lsp-imenu)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-quickhelp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(company-quickhelp-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'evil)
;;(evil-mode 1)
;;(setq evil-default-state 'emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit
;; 在 a-string 两边加上 " 或者 (), 只要将光标放置于 a-string 开头按下 M-( 或者 M-" 即可
;; (hello world) 光标放置于 hello world 中间按下 M-S 即可将其分割成 (hello) (world)
;; 按下 M-J 可以将其重新连接起来， 字符串也一样
;; C-(, C-) 吃掉左边或者右边的 s-exp, C-{, C-} 吐出来
;; M-r 跳出外围块(去掉外层代码)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'elisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'common-lisp-mode-hook 'enable-paredit-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .cquery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cquery-init-file ()
  (interactive)
  (if (string-equal (file-name-nondirectory (buffer-file-name)) ".cquery")
      (insert-file-contents "~/模板/.cquery")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'auto-complete-config)
;;;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/Extensions/ac-dict")
;;(ac-config-default)
;;(setq ac-ignore-case 0)
;;
;;(set-default 'ac-sources
;;	     '(ac-source-abbrev
;;	       ac-source-dictionary
;;	       ac-source-words-in-same-mode-buffers
;;	       ac-source-semantic
;;	       ac-source-yasnippet))
;;
;; (define-key ac-mode-map (kbd "M-/") 'ac-complete-semantic)


;;;;;;;;;;;;;;;;
;; tabbar.el
;;;;;;;;;;;;;;;;
;(require 'tabbar)
;(tabbar-mode)
;(global-set-key (kbd "s-h") 'tabbar-backward-group)
;(global-set-key (kbd "s-l") 'tabbar-forward-group)
;(global-set-key (kbd "s-j") 'tabbar-backward)
;(global-set-key (kbd "s-k") 'tabbar-forward)


(message "Loading extensions...done")
