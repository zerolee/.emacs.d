;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scheme-program-name "scheme")         ;; 如果用 Petite 就改成 "petite"
(setq geiser-active-implementations '(chez))
;;(setq geiser-active-implementations '(racket))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'elisp-mode-hook
	  '(lambda ()
	     (semantic-mode t)
	     (semantic-idle-summary-mode)
	     (require 'semantic/sb)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inferior-lisp-program "/usr/bin/sbcl")
;;(require 'slime)
;;(slime-setup)
;;(slime-setup '(slime-fancy))
 
(add-hook 'common-lisp-mode-hook
	  '(lambda ()
	     (require 'slime)
	     (slime-setup)
	     (slime-setup '(slime-fancy))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C programming language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-hook
	  '(lambda ()
	     (c-set-style "cc-mode")))

;; 查看当前所在函数名称
(add-hook 'c-mode-common-hook
	  '(lambda()
	     (which-function-mode t)))


;; cquery
;; xref-find-definitions ( M-. )
;; xref-find-references  ( M-? )
;; xref-find-apropos     ( C-M-. )
(setq cquery-executable "/home/zmqc/backups/src/cquery/cquery/build/release/bin/cquery")

(add-hook 'c-mode-hook
	  '(lambda ()
	     (require 'cquery)
	     (lsp-cquery-enable)
     	     (require 'company-lsp)
	     (push 'company-lsp company-backends)
     	     (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
	     (setq cquery--get-init-params '(:completion (:detailedLabel t)))
	     (require 'ivy-xref)
	     (setq xref-show-xrefs-function 'ivy-xref-show-xrefs)
	     (setq cquery-sem-highlight-method 'overlay)
	     (setq cquery-sem-highlight-method 'font-lock)))

(require 'lsp-imenu)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'asm-mode-hook
	  '(lambda()
	     (which-func-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 使用 antlr mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'antlr-v4-mode "antlr-mode" nil t)
(push '("\\.g4\\'" . antlr-v4-mode) auto-mode-alist)
