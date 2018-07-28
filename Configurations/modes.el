;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme  geiser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package geiser
  :config
  (setq scheme-program-name "scheme"
        geiser-active-implementations '(chez)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package slime
  :hook (lisp-mode . slime-lisp-mode-hook)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"
        slime-contribs '(slime-fancy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lsp-common-set ()
  (use-package lsp-ui
    :config
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-sideline-enable nil)
    (lsp-ui-mode)
    (define-key lsp-ui-mode-map [remap xref-find-references]
      #'lsp-ui-peek-find-references))
  (use-package flycheck
    :bind (:map flycheck-mode-map
                ("M-g l" . flycheck-list-errors))
    :config
    (flycheck-mode))
  (use-package ivy-xref
    :init
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
  (set (make-local-variable 'company-backends)
       '(company-lsp  company-dabbrev-code
                      company-dabbrev
                      company-files))
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
  (global-set-key (kbd "S-<f2>") #'lsp-rename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C programming language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cquery
;; xref-find-definitions ( M-. )
;; xref-find-references  ( M-? )
;; xref-find-apropos     ( C-M-. )
(use-package cquery
  :commands lsp-cquey-enable
  :init
  (setq cquery-executable "/home/zmqc/bin/cquery")
  (add-hook 'c-mode-hook
            '(lambda ()
               (use-package company-lsp
                 :config
                 (setq company-transformers nil company-lsp-async t
                       company-lsp-cache-candidates nil))
               (lsp-cquery-enable)
               (setq cquery--get-init-params
                     '(:index (:comment 2) :cacheFormat "msgpack"
                              :completion (:detailedLabel t)))
               (lsp-common-set))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp-java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-java
  :commands lsp-java-enable
  :init
  (setq lsp-java--workspace-folders
        (list "/home/zmqc/study/java/tmp/"
              "/home/zmqc/study/java/javaemacs/")
        lsp-java-server-install-dir
        "/home/zmqc/backups/src/jdt-language-server-latest/")
  (add-hook 'java-mode-hook
            '(lambda ()
               (use-package company-lsp)
               (lsp-java-enable)
               (lsp-common-set))))

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
              ("M-g p" . flymake-goto-prev-error)
              ("M-g n" . flymake-goto-next-error)
              ("M-g l" . flymake-show-diagnostics-buffer)))
