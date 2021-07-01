;;; programs.el --- 编程相关的一些配置 -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'use-package)
(require 'diminish)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme  geiser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package geiser-guile
  :defer t)

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
  :defines (zerolee-emmet-first-backtab zerolee-emmet-edit-ring)
  :config
  (defun zerolee--emmet-newline-and-indent ()
    (require 'sgml-mode)
    (unless (and (eq (char-before) ?>)
                 (eq (char-after) ?\C-j))
      (if (nth 4 (syntax-ppss))
          (search-forward "-->")
        (sgml-skip-tag-forward 1)
        (when (looking-back "</body>" (- (point) 7))
          (move-end-of-line 0))))
    (while (or (looking-back "li>\\|dd>\\|th>\\|option>\\|td>\\|tr>\\|tbody>"
                             (- (point) 8))
               (looking-at "[ \t\n]*</\\(map\\|nav\\)>")
               (and (looking-back "</a>" (- (point) 4))
                    (looking-at "[ \t\n]*</li>")))
      (sgml-skip-tag-forward 1))
    (unless (region-active-p)
      (newline-and-indent 1)))
  (defun zerolee--emmet-maybe-expand ()
    "1. 在合适的位置调用 emmet 进行展开.
2. 在需要 indent 的地方进行 indent.
3. 在需要将光标移动到下一个编辑点时移动到下一个编辑点.
4. 在需要新起一行的时候新起一行."
    (interactive)
    (cond ((eq major-mode 'js-mode)
           (let ((p (point)))
             (call-interactively #'indent-for-tab-command)
             (when (= p (point))
               (if (memq (char-after) '(?\C-j nil ? ))
                   (if (memq (char-before) '(?  ?\t ?\; ?\}))
                       (if (and (eq (char-before) ?\;)
                                (looking-at "[ \t\n]*}"))
                           (up-list 1)
                         (sgml-skip-tag-forward 1)
                         (when (and (looking-back "</script>" (- (point) 9))
                                    (looking-at "[ \t\n]*</head>"))
                           (sgml-skip-tag-forward 1)
                           (when (looking-at "[ \t\n]*<body")
                             (search-forward ">")))
                         (newline-and-indent 1))
                     (let ((cbs (flatten-list company-backends)))
                       (while cbs
                         (catch 'done
                           (let ((backends (car cbs)))
                             (condition-case nil
                                 (call-interactively backends)
                               (error
                                (setq cbs (cdr cbs))
                                (throw 'done 0)))
                             (setq cbs nil))))))
                 (when (nth 3 (syntax-ppss))
                   (up-list 2 t))
                 (when (eq (char-after) ?\))
                   (call-interactively #'forward-char))
                 (when (memq (char-after) '(?\; ?\.))
                   (call-interactively #'forward-char))))))
          ((and (or (memq (char-after) '(?\C-j nil ? ))
                    (and (eq (char-after) ?<)
                         (looking-back " [a-z]+" (- (point) 5))))
                (not (memq (char-before) '(?\C-j ?> ?\" ? )))
                (not (nth 3 (syntax-ppss)))
                (not (nth 4 (syntax-ppss)))
                (not (looking-back "<[a-z]+" (line-beginning-position))))
           (unless (call-interactively #'emmet-expand-line)
             (end-of-line)
             (newline-and-indent 1)))
          ((or (and (looking-at "<[/a]")
                    (not (looking-back "^[ \t]+" (line-beginning-position))))
               (and (nth 3 (syntax-ppss))
                    (or (eq (char-after) ?\") (eq (char-after) ?\')
                        (and (eq (char-before) ?\') (eq (char-after) ?\))))))
           (condition-case nil
               (if (and zerolee-emmet-first-backtab
                        (> zerolee-emmet-first-backtab (point-marker)))
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
  (defun zerolee--emmet-backtab () (interactive)
         (unless zerolee-emmet-first-backtab
           (setq zerolee-emmet-first-backtab (point-marker)))
         (while (and zerolee-emmet-edit-ring
                     (> (car zerolee-emmet-edit-ring)
                        (point)))
           (pop zerolee-emmet-edit-ring))
         (if zerolee-emmet-edit-ring
             (goto-char (pop zerolee-emmet-edit-ring))
           (call-interactively #'emmet-prev-edit-point)))
  (defun zerolee--emmet-company-abbrev ()
    "调用 company-abbrev."
    (interactive)
    (if vesie-mode
        (save-excursion
          (end-of-line)
          (newline-and-indent)
          (yank))
      (call-interactively #'company-abbrev)))
  (defun zerolee--emmet-expand-line ()
    "调用 emmet-expand-line."
    (interactive)
    (if vesie-mode
        (save-excursion
          (beginning-of-line)
          (open-line 1)
          (call-interactively #'indent-for-tab-command)
          (yank))
      (call-interactively #'emmet-expand-line)))
  (define-key emmet-mode-keymap (kbd "<tab>") #'zerolee--emmet-maybe-expand)
  (define-key emmet-mode-keymap (kbd "<backtab>") #'zerolee--emmet-backtab)
  (define-key emmet-mode-keymap (kbd "M-p") #'zerolee--emmet-expand-line)
  (define-key emmet-mode-keymap (kbd "M-n") #'zerolee--emmet-company-abbrev)
  (add-hook 'sgml-mode-hook
            #'(lambda ()
                (setq-local company-backends
                            '((company-dabbrev-code company-yasnippet)
                              company-keywords
                              company-files
                              company-dabbrev))
                (setq-local zerolee-emmet-edit-ring nil)
                (setq-local zerolee-emmet-first-backtab nil)))
  (add-hook 'mhtml-mode-hook
            #'(lambda ()
                (require 'yasnippet)
                (yas-activate-extra-mode 'js-mode)
                (yas-activate-extra-mode 'css-mode)
                (yas-deactivate-extra-mode 'js-mode)
                (yas-deactivate-extra-mode 'css-mode)
                (setq-local electric-pair-inhibit-predicate
                            #'(lambda (char)
                                (or (and (eq major-mode 'js-mode)
                                         (= char ?<))
                                    (electric-pair-default-inhibit char))))))
  (when (file-exists-p "~/.emacs.d/abbrev/mhtml-mode/abbrev_defs")
    (read-abbrev-file "~/.emacs.d/abbrev/mhtml-mode/abbrev_defs")))

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
                    (error (zerolee-go))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; citre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package citre
  :init
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
  :config
  (require 'init-tools)
  (setq citre-project-root-function #'zerolee--get-project-root)
  (defun citre-core--get-dir-os (ptag-cwd tagsfile)
    (let* ((dir (or ptag-cwd
                    (gethash tagsfile citre-core--tags-file-cwd-guess-table)
                    (if citre-core--dont-prompt-for-cwd
                        (file-name-directory tagsfile)
                      (zerolee--get-project-root))))
           (dir (expand-file-name dir))
           (dir-local (file-local-name dir)))
      (unless (eq (aref dir-local 0) ?/)
        (setf (aref dir-local 0) (upcase (aref dir-local 0))))
      (cons
       (if-let ((remote-id (file-remote-p tagsfile)))
           (concat remote-id dir-local)
         dir-local)
       (pcase (aref dir-local 0)
         (?/ 'unix)
         (_ 'nt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 使用正则或者 tags 进行跳转补全
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zerolee-jump-config ()
  "用来配置代码变量、函数的跳转."
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)
  (require 'citre-util)
  (when (citre-tags-file-path)
    (add-hook 'xref-backend-functions #'citre-xref-backend nil t)
    (add-hook 'completion-at-point-functions
              #'citre-completion-at-point -100 t)
    (setq-local imenu-create-index-function
                #'citre-imenu-create-index-function))
  (when (eq major-mode 'js-mode)
    (define-key js-mode-map (kbd "M-.") #'xref-find-definitions)))

(add-hook 'prog-mode-hook
          #'(lambda ()
              (when (not (derived-mode-p 'lisp-data-mode))
                (zerolee-jump-config))))

(add-hook 'sgml-mode-hook #'zerolee-jump-config)

(provide 'programs)
;;; programs.el ends here
