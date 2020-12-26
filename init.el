;;; init.el: --- Emacs 启动时加载-*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(setq load-path
      (cons "~/.emacs.d/Configurations"
            (cons "~/.emacs.d/Extensions" load-path)))

(setq custom-file "~/.emacs.d/Configurations/custom.el")

(require 'emacs-std)
(require 'extensions)
(require 'programs)
(require 'my-macros)
(require 'my-hydra)
(require 'bindings)
(load custom-file)

(provide 'init)
;;; init.el ends heres
