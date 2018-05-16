;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme  geiser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package geiser
  :config
  (progn
    (setq scheme-program-name "scheme"
          geiser-active-implementations '(chez))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package slime
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lsp-common-set ()
  (use-package company-lsp
    :config
    (setq company-transformers nil company-lsp-async t
          company-lsp-cache-candidates nil)
    (set (make-local-variable 'company-backends)
         '(company-lsp  company-dabbrev-code
                        company-dabbrev
                        company-files)))
  (use-package lsp-ui
    :config
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-sideline-enable nil)
    (lsp-ui-mode))
  (use-package flycheck
    :config
    (flycheck-mode))
  (use-package ivy-xref
    :init
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C programming language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cquery
;; xref-find-definitions ( M-. )
;; xref-find-references  ( M-? )
;; xref-find-apropos     ( C-M-. )
(setq cquery-executable "/home/zmqc/backups/src/cquery/build/release/bin/cquery")

(use-package cquery
  :init
  (add-hook 'c-mode-hook
            '(lambda ()
               (lsp-common-set)
               (lsp-cquery-enable)
               (setq cquery--get-init-params '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp-java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lsp-java--workspace-folders (list "/home/zmqc/study/java/tmp/"
                                        "/home/zmqc/study/java/javaemacs/"))
(setq lsp-java-server-install-dir "/home/zmqc/backups/src/jdt-language-server-latest/")

(use-package lsp-java
  :init
  (add-hook 'java-mode-hook
            '(lambda ()
               (lsp-common-set)
               (lsp-java-enable))))

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
              ("S-<f2>" . eglot-rename)))
