;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme  geiser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package geiser
  :ensure t
  :config
  (progn
    (setq scheme-program-name "scheme")        
    (setq geiser-active-implementations '(chez))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun company-lsp-common-set ()
  (use-package company-lsp
    :ensure t
    :config
    (setq company-transformers nil company-lsp-async t
	  company-lsp-cache-candidates nil)
    (set (make-local-variable 'company-backends)
	 '(company-lsp  company-dabbrev-code
			company-dabbrev
			company-files))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C programming language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cquery
;; xref-find-definitions ( M-. )
;; xref-find-references  ( M-? )
;; xref-find-apropos     ( C-M-. )
(setq cquery-executable "/home/zmqc/backups/src/cquery/build/release/bin/cquery")

(use-package cquery
  :ensure t
  :init
  (add-hook 'c-mode-hook
	    '(lambda ()
	       (company-lsp-common-set)
	       (lsp-cquery-enable)
	       (setq cquery--get-init-params '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp-java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lsp-java--workspace-folders (list "/home/zmqc/study/java/tmp/"
					"/home/zmqc/study/java/javaemacs/"))
(setq lsp-java-server-install-dir "/home/zmqc/backups/src/jdt-language-server-latest/")

(use-package lsp-java
  :ensure t
  :init
  (add-hook 'java-mode-hook
	    '(lambda ()
	       (company-lsp-common-set)
	       (lsp-java-enable))))

(use-package ivy-xref
  :ensure t
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package lsp-imenu
  :init
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 使用 antlr mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package antlr-mode
  :mode ("\\.g4\\'" . antlr-v4-mode))
