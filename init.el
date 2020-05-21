;; -*- coding: utf-8; lexical-binding: t; -*-
(setq load-path
      (cons "~/.emacs.d/Configurations"
            (cons "~/.emacs.d/Extensions" load-path)))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq gc-cons-threshold (* 128 1024 1024)
      file-name-handler-alist nil
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold (* 32 1024 1024)
                   gc-cons-percentage 0.3)
             (garbage-collect)) t)

(setq custom-file
      (expand-file-name "Configurations/custom.el" user-emacs-directory))

(load "emacs-std")
(load "extensions")
(load "programs")
(load "my-macros")
(load "my-hydra")
(load "bindings")
(load-file custom-file)
