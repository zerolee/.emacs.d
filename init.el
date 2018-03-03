;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; 我从 elpa 下载的包：(company, helm, yasnippet, yasnippet-snippets, auto-complete, company-quickhelp, helm-company)
(package-initialize)

(setq load-path
      (cons "~/.emacs.d/Extensions/emacs-groovy-mode"
			(cons "~/.emacs.d/Configurations"
			      (cons "~/.emacs.d/Extensions" load-path))))

;; This file is for standard Emacs option.
(load "emacs-std")

;; This file includes the keybindings for your Emacs setup
(load "bindings")

;; This file can contain all the macros you have developed
(load "my-macros")

;; This can contain configuration for the different major modes
(load "modes")

;; This can contain extensions you have use
(load "extensions")



;;;;;;;;;;;;;;;;;;;
;; exwm
;;;;;;;;;;;;;;;;;;;
;;(require 'exwm)
;;(require 'exwm-config)
;;(exwm-config-default)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel wgrep paredit geiser yasnippet-snippets markdown-mode exwm evil cquery company-quickhelp company-lsp ivy-xref smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
