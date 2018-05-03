(setq load-path
      (cons "~/.emacs.d/Configurations"
            (cons "~/.emacs.d/Extensions" load-path)))

(setq gc-cons-threshold 100000000)

(setq custom-file (expand-file-name "Configurations/custom.el" user-emacs-directory))

;; This file is for standard Emacs option.
(load "emacs-std")

;; This can contain extensions you have use
(load "extensions")

;; This can contain configuration for the different major modes
(if window-system  (load "modes"))

;; This file can contain all the macros you have developed
(load "my-macros")

;;;
(load "hydras")

;; This file includes the keybindings for your Emacs setup
(load "bindings")

(load-file custom-file)
(setq gc-cons-threshold 4000000)
