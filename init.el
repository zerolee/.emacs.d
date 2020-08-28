;; -*- coding: utf-8; lexical-binding: t; -*-
(setq load-path
      (cons "~/.emacs.d/Configurations"
            (cons "~/.emacs.d/Extensions" load-path)))

(setq custom-file
      (expand-file-name "Configurations/custom.el" user-emacs-directory))

(load "emacs-std")
(load "extensions")
(load "programs")
(load "my-macros")
(load "my-hydra")
(load "bindings")
(load-file custom-file)

(provide 'init)
;;; init.el ends heres
