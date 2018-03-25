;; (counsel wgrep paredit geiser yasnippet-snippets markdown-mode exwm evil cquery company-quickhelp company-lsp ivy-xref smex)
(setq load-path
      (cons "~/.emacs.d/Extensions/emacs-groovy-mode"
			(cons "~/.emacs.d/Configurations"
			      (cons "~/.emacs.d/Extensions" load-path))))

(setq custom-file (expand-file-name "Configurations/custom.el" user-emacs-directory))

;; This file is for standard Emacs option.
(load "emacs-std")


;; This can contain extensions you have use
(load "extensions")

;; This can contain configuration for the different major modes
(load "modes")

;; This file can contain all the macros you have developed
(load "my-macros")

;; This file includes the keybindings for your Emacs setup
(load "bindings")

(load-file custom-file)
