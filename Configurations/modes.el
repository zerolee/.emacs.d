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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C programming language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cquery
;; xref-find-definitions ( M-. )
;; xref-find-references  ( M-? )
;; xref-find-apropos     ( C-M-. )
(setq cquery-executable "/home/zmqc/backups/src/cquery/cquery/build/release/bin/cquery")

(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (lsp-cquery-enable)
     	     (require 'company-lsp)
     	     (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
	     (setq cquery--get-init-params '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))
	     (require 'ivy-xref)
	     (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
	     (setq cquery-sem-highlight-method 'font-lock)
	     (set (make-local-variable 'company-backends)
		  '((company-lsp company-yasnippet ) company-dabbrev-code
		    company-dabbrev
		    company-files))))


(use-package lsp-imenu
  :init
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 使用 antlr mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package antlr-mode
  :mode ("\\.g4\\'" . antlr-v4-mode))

