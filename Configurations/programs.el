;;; programs.el --- 编程相关的一些配置 -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'use-package)
(require 'diminish)
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
  (when (eq major-mode 'css-mode)
    (setq-local lsp-diagnostics-provider :none)
    (eldoc-mode -1))
  (lsp)
  (setq-local company-backends
              '((company-yasnippet company-capf)
                company-dabbrev-code company-dabbrev
                company-files company-keywords))
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
  (advice-add 'lsp-completion--filter-candidates :around
              #'(lambda (fun &rest arg)
                  (let ((case-fold-search nil))
                    (apply fun arg))))
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
  :hook ((js-mode css-mode) . lsp--common-set))

(use-package emmet-mode
  :ensure nil
  :diminish emmet-mode
  :hook (sgml-mode css-mode)
  :bind (:map emmet-mode-keymap
              ("C-c e p" . emmet-prev-edit-point)
              ("C-c e n" . emmet-next-edit-point)
              ("C-j" . nil))
  :custom
  (emmet-move-cursor-between-quotes t)
  :config
  (defun zerolee--emmet-newline-and-indent ()
    (require 'sgml-mode)
    (sgml-skip-tag-forward 1)
    (if (or (looking-back "</td>" (- (point) 5))
            (and (looking-back "</a>" (- (point) 4))
                 (looking-at "[ \t\n]*</li>")))
        (sgml-skip-tag-forward 2))
    (if (or (looking-back "</li>" (- (point) 5))
            (looking-back "</dd>" (- (point) 5)))
        (sgml-skip-tag-forward 1))
    (newline-and-indent 1))
  (defsubst zerolee--emmet-maybe-expand ()
    (interactive)
    (cond ((and (memq (char-after) '(?\C-j nil ? ))
                (not (memq (char-before) '(?\C-j ?> ?\" ? )))
                (not (nth 3 (syntax-ppss)))
                (not (looking-back "<[a-z]+" (line-beginning-position))))
           (call-interactively #'emmet-expand-line))
          ((or (and (looking-at "<[/a]")
                    (not (looking-back "^[ \t]+" (line-beginning-position))))
               (and (nth 3 (syntax-ppss))
                    (or (eq (char-after) ?\") (eq (char-after) ?\')
                        (and (eq (char-before) ?\') (eq (char-after) ?\))))))
           (condition-case nil
               (if (and zerolee-emmet-first-backtab
                        (/= zerolee-emmet-first-backtab (point-marker)))
                   (progn
                     (push (point-marker) zerolee-emmet-edit-ring)
                     (setq zerolee-emmet-edit-ring
                           (seq-uniq zerolee-emmet-edit-ring))
                     (goto-char zerolee-emmet-first-backtab)
                     (setq zerolee-emmet-first-backtab nil))
                 (push (point-marker) zerolee-emmet-edit-ring)
                 (setq zerolee-emmet-edit-ring
                       (seq-uniq zerolee-emmet-edit-ring))
                 (setq zerolee-emmet-first-backtab nil)
                 (call-interactively #'emmet-next-edit-point))
             (error
              (let ((point (point)))
                (call-interactively #'indent-for-tab-command)
                (if (= point (point))
                    (zerolee--emmet-newline-and-indent))))))
          (t
           (let ((point (point)))
             (call-interactively #'indent-for-tab-command)
             (if (= point (point))
                 (condition-case nil
                     (call-interactively #'emmet-next-edit-point)
                   (error
                    (zerolee--emmet-newline-and-indent))))))))
  (defsubst zerolee--emmet-backtab () (interactive)
    (unless zerolee-emmet-first-backtab
      (setq zerolee-emmet-first-backtab (point-marker)))
    (while (and zerolee-emmet-edit-ring
                (> (car zerolee-emmet-edit-ring)
                   (point)))
      (pop zerolee-emmet-edit-ring))
    (if zerolee-emmet-edit-ring
        (goto-char (pop zerolee-emmet-edit-ring))
      (call-interactively #'emmet-prev-edit-point)))
  (define-key emmet-mode-keymap (kbd "<tab>") #'zerolee--emmet-maybe-expand)
  (define-key emmet-mode-keymap (kbd "<backtab>") #'zerolee--emmet-backtab)
  (add-hook 'sgml-mode-hook
            #'(lambda ()
                (setq-local company-backends
                            '(company-yasnippet company-dabbrev-code
                                                company-keywords
                                                company-files
                                                company-dabbrev))
                (setq-local zerolee-emmet-edit-ring nil)
                (setq-local zerolee-emmet-first-backtab nil))))

(use-package js-comint
  :ensure nil
  :diminish js-comint
  :hook (js-mode . (lambda ()
                     (local-set-key (kbd "C-x C-e") #'js-eval-last-sexp)
                     (local-set-key (kbd "C-M-x") #'js-eval-current-defun)))
  :commands (js-eval-last-sexp js-eval-current-defun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 使用 antlr mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'antlr-v4-mode "antlr-mode" nil t)
(push '("\\.g4\\'" . antlr-v4-mode) auto-mode-alist)

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
                            company-files company-keywords))
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
