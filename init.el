;; -*- coding: utf-8; lexical-binding: t; -*-
(setq load-path
      (cons "~/.emacs.d/Configurations"
            (cons "~/.emacs.d/Extensions" load-path)))

(setq custom-file "~/.emacs.d/Configurations/custom.el")

(load "emacs-std")
(load "extensions")
(load "programs")
(load "my-macros")
(load "my-hydra")
(load "bindings")
(load custom-file)

(provide 'init)
;;; init.el ends heres
