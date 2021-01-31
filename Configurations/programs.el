;;; programs.el --- 编程相关的一些配置 -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme  geiser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package geiser
  :defer t
  :config
  (setq scheme-program-name "guile"
        geiser-active-implementations '(guile)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp sly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sly
  :defer t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"
        sly-complete-symbol-function 'sly-simple-completions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp 相关的通用配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lsp--common-set ()
  "Lsp 的一些通用配置."
  (lsp)
  (setq-local company-backends
              '((company-yasnippet company-capf)
                company-dabbrev-code company-dabbrev
                company-files))
  (setq-local read-process-output-max (* 1024 1024))
  (setq lsp-enable-on-type-formatting nil
        lsp-auto-execute-action nil
        lsp-auto-configure nil)
  (add-hook 'completion-at-point-functions #'lsp-completion-at-point nil t)
  (define-key lsp-mode-map (kbd "S-<f2>") #'lsp-rename)
  (define-key lsp-mode-map (kbd "M-.") #'xref-find-definitions)
  (define-key lsp-mode-map (kbd "M-?") #'xref-find-references)
  (define-key lsp-mode-map (kbd "C-h .")
    #'(lambda ()
        (interactive)
        (if (get-buffer-window "*lsp-help*")
            (delete-windows-on "*lsp-help*")
          (lsp-describe-thing-at-point))))
  (define-key lsp-mode-map (kbd "s-l") nil)
  (setq abbrev-mode nil)
  (lsp-diagnostics-mode 1)
  (advice-add 'lsp-completion--regex-fuz :around
              #'(lambda (_orig-func str)
                  (format "^%s" str)))
  (add-hook 'lsp-on-idle-hook #'lsp--document-highlight nil t)
  (lsp-enable-imenu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C programming language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cquery
;; xref-find-definitions ( M-. )
;; xref-find-references  ( M-? )
;; xref-find-apropos     ( C-M-. )
(use-package ccls
  :defer t
  :hook ((c-mode c++-mode objc-mode) . (lambda ()
                                         (require 'ccls)
                                         (lsp--common-set))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp-java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-java
  :defer t
  :init
  (setq lsp-java-server-install-dir
        "~/backups/src/jdt-language-server-latest/")
  :hook (java-mode . (lambda ()
                       (require 'lsp-java)
                       (lsp--common-set))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html javascript css
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :commands lsp
  :hook ((js-mode css-mode html-mode web-mode) . lsp--common-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 使用 antlr mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'antlr-v4-mode "antlr-mode" nil t)
(push '("\\.g4\\'" . antlr-v4-mode) auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eglot
  :bind (:map eglot-mode-map
              ("S-<f2>" . eglot-rename)
              ("M-." . xref-find-definitions)
              ("M-?" . xref-find-references)
              ("C-h ." . (lambda ()
                           (interactive)
                           (if (get-buffer-window eldoc--doc-buffer)
                               (delete-windows-on eldoc--doc-buffer)
                             (eldoc-doc-buffer)))))
  :hook ((eglot-managed-mode
          .
          (lambda ()
            (when (eglot-managed-p)
              (setq-local company-backends
                          '((company-yasnippet company-capf)
                            company-dabbrev-code company-dabbrev
                            company-files))
              (setq-local completion-styles
                          '(basic partial-completion emacs22)))))
         (eglot-server-initialized
          .
          (lambda (_server)
            (add-hook 'xref-backend-functions 'dumb-jump-xref-activate nil t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markdown-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cmake-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cmake-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dumb-jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dumb-jump
  :defer t
  :init
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ivy)
  :config
  (advice-add 'dumb-jump-get-project-root :around
              #'(lambda (func filepath)
                  (let ((dumb-jump-default-project default-directory))
                    (funcall func filepath))))
  (advice-add 'xref-find-definitions :around
              #'(lambda (func identifier)
                  (condition-case nil
                      (funcall func identifier)
                    (error (zerolee-go)))))
  (set-default 'xref-backend-functions
               (push #'dumb-jump-xref-activate
                     (default-value 'xref-backend-functions))))

(provide 'programs)
;;; programs.el ends here
