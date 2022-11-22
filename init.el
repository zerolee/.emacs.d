;;; init.el: --- Emacs 启动时加载-*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(setq load-path
      (cons "~/.emacs.d/Configurations"
            (cons "~/.emacs.d/Extensions" load-path)))

(setq custom-file "~/.emacs.d/Configurations/custom.el")
(require 'use-package)
(setq use-package-always-ensure t)
(use-package diminish)

(require 'zerolee-lib)
(require 'init-std)
(require 'init-extensions)
(require 'init-programs)
(require 'init-macros)
(require 'init-transient)
(require 'init-bindings)
(load custom-file)

(provide 'init)
;;; init.el ends heres
