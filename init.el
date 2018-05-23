(setq load-path
      (cons "~/.emacs.d/Configurations"
            (cons "~/.emacs.d/Extensions" load-path)))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq gc-cons-threshold 100000000
      file-name-handler-alist nil)
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(setq custom-file (expand-file-name "Configurations/custom.el" user-emacs-directory))

;; This file is for standard Emacs option.
(load "emacs-std")

;; This can contain extensions you have use
(load "extensions")

;; This can contain configuration for the different major modes
(load "modes")

;; This file can contain all the macros you have developed
(load "my-macros")

;;;
(load "hydras")

;; This file includes the keybindings for your Emacs setup
(load "bindings")

(load-file custom-file)
