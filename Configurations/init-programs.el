;;; init-programs.el --- 编程相关的一些配置 -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'diminish)
(require 'init-tools)


(defvar-local last-symbol nil)
(defun zerolee-help-doc (buffer document)
  "文档的启动与关闭."
  (let ((current-symbol (thing-at-point 'symbol t)))
    (if (or (string= current-symbol last-symbol)
            (null current-symbol))
        (if (get-buffer-window buffer)
            (delete-windows-on buffer)
          (call-interactively document))
      (call-interactively document))
    (setq-local last-symbol current-symbol)))
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
  :bind (:map sly-mode-map
              ("C-h ."
               .
               (lambda ()
                 (interactive)
                 (zerolee-help-doc "*sly-description*" #'sly-documentation))))
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"
        sly-complete-symbol-function 'sly-simple-completions))

(use-package emmet-mode
  :ensure nil
  :diminish emmet-mode
  :hook (sgml-mode css-mode css-ts-mode html-ts-mode)
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
  (zerolee-set-key emmet-mode-keymap
    ("<tab>" #'zerolee--emmet-maybe-expand)
    ("<backtab>" #'zerolee--emmet-backtab)
    ("M-p" #'zerolee--emmet-expand-line)
    ("M-n" #'zerolee--emmet-company-abbrev))
  (add-hook 'sgml-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-yasnippet
                             company-dabbrev-code
                             company-keywords)
                            company-files
                            company-dabbrev))
              (setq-local zerolee-emmet-edit-ring nil)
              (setq-local zerolee-emmet-first-backtab nil)))
  (add-hook 'mhtml-mode-hook
            (lambda ()
              (require 'yasnippet)
              (yas-activate-extra-mode 'js-mode)
              (yas-activate-extra-mode 'css-mode)
              (yas-deactivate-extra-mode 'js-mode)
              (yas-deactivate-extra-mode 'css-mode)
              (setq-local electric-pair-inhibit-predicate
                          (lambda (char)
                            (or (and (eq major-mode 'js-mode)
                                     (= char ?<))
                                (electric-pair-default-inhibit char))))))
  (when (file-exists-p "~/.emacs.d/abbrev/mhtml-mode/abbrev_defs")
    (read-abbrev-file "~/.emacs.d/abbrev/mhtml-mode/abbrev_defs")))

(use-package js-comint
  :ensure nil
  :diminish js-comint
  :config
  (zerolee-set-key js-mode-map js-ts-mode-map
    ([remap eval-last-sexp] #'js-comint-send-last-sexp)
    ("C-M-x" 'js-eval-current-defun))
  :commands (js-eval-last-sexp js-eval-current-defun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 使用 antlr mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'antlr-v4-mode "antlr-mode" nil t)
(push '("\\.g4\\'" . antlr-v4-mode) auto-mode-alist)
(add-hook 'antlr-mode-hook
          (lambda ()
            (let ((added (expand-file-name
                          "~/bin/config/antlr-4.9.3-complete.jar:"))
                  (env (getenv "CLASSPATH")))
              (unless (and env (string-match added env))
                (setenv "CLASSPATH" (concat ".:" added env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("S-<f2>" . eglot-rename)
              ("M-." . xref-find-definitions)
              ("M-?" . xref-find-references)
              ("C-h ."
               .
               (lambda ()
                 (interactive)
                 (zerolee-help-doc eldoc--doc-buffer #'eldoc-doc-buffer))))
  :custom
  (eglot-events-buffer-size 0)
  :hook ((eglot-managed-mode
          .
          (lambda ()
            (when (eglot-managed-p)
              (setq-local company-backends
                          '((company-yasnippet company-capf
                                               company-dabbrev-code)
                            company-files company-keywords
                            company-dabbrev))
              (setq completion-category-defaults
                    (remove '(eglot (styles flex basic))
                            completion-category-defaults)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markdown-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dumb-jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dumb-jump
  :init
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ivy)
  :config
  (advice-add 'dumb-jump-get-project-root :around
              (lambda (func filepath)
                (let ((dumb-jump-default-project (zerolee--get-project-root)))
                  (funcall func filepath))))
  (advice-add 'xref-find-definitions :around
              (lambda (func identifier)
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
(defconst zerolee--ctags-alist
  '((emacs-lisp-mode    . "EmacsLisp")
    (sh-mode            . "Sh")
    ("\\.html?\\'"      . "HTML,JavaScript,CSS")
    ("\\.h\\'"          . "C")
    ("\\.c\\'"          . "C")
    ("\\.hh\\'"         . "C++")
    ("\\.[Cc]+[Pp]*\\'" . "C++")
    ("\\.[Ss]\\'"       . "Asm")
    ("\\.cs\\'"         . "C#")
    ("\\.css\\'"        . "HTML,JavaScript,CSS")
    ("\\.cuf\\'"        . "Fortran")
    ("\\.clj\\'"        . "Clojure")
    ("CMakeLists\\.txt\\'". "CMake")
    ("\\.[Ff]\\'"       . "Fortran")
    ("\\.[Ff]90\\'"     . "Fortran")
    ("\\.go\\'"         . "Go")
    ("\\.java\\'"       . "Java")
    ("\\.js\\'"         . "HTML,JavaScript,CSS")
    ("\\.lua\\'"        . "Lua")
    ("\\.lisp\\'"       . "Lisp")
    ("\\.m\\'"          . "M4")
    ("\\.php\\'"        . "PHP")
    ("\\.pl\\'"         . "Perl")
    ("\\.p[l]?6\\'"     . "Perl6")
    ("\\.py\\'"         . "Python")
    ("\\.raku\\'"       . "Perl6")
    ("\\.rb\\'"         . "Ruby")
    ("\\.rs\\'"         . "Rust")
    ("\\.scm\\'"        . "Scheme")
    ("\\.scss\\'"       . "SCSS")
    ("[Mm]akefile\\'"   . "Make")
    ("\\.tex\\'"        . "Tex"))
  "每个元素由 (REGEXP . STRING) or (MAJOR-MODE . STRING) 构成.")

(defvar-local zerolee-timestamp (current-time) "保存上次更新 tags 的时间戳.")
(defvar-local zerolee-ctags-command nil "生成相应的 tags 文件的命令.")
(defvar-local zerolee-ctags-directory nil "生成相应的 tags 文件所在目录.")
(defun zerolee-regenerate-ctags (&optional arg)
  "生成相应的 tags 文件，传入参数时设置生成 tags 文件的目录."
  (interactive "P")
  ;; 计算出当前 buffer 所使用的语言，然后将其跟命令合并.
  (unless zerolee-ctags-command
    (setq-local zerolee-ctags-command
                (format "ctags --languages=%s --kinds-all='*' --fields='*' --extras='*' -R &"
                        (or
                         (catch 'done
                           (dolist (alist zerolee--ctags-alist)
                             (when (or (and (symbolp (car alist))
                                            (eq (car alist) major-mode))
                                       (and (stringp (car alist))
                                            (string-match (car alist) (buffer-file-name))))
                               (throw 'done (cdr alist)))))
                         "languages"))))
  ;; 如果检测不到 tags 文件那么手动生成.
  (unless (citre-tags-file-path)
    (setq-local zerolee-ctags-command
                (read-from-minibuffer "command: " zerolee-ctags-command))
    (zerolee--tags-config t))
  ;; 如果传入了参数，则设定一个路径后在执行命令
  (let ((default-directory
         (or
          (if arg
              (setq-local zerolee-ctags-directory
                          (read-directory-name
                           "Select directory: " default-directory))
            (and
             (citre-tags-file-path)
             (string-match (file-name-directory (citre-tags-file-path)) default-directory)
             (setq-local zerolee-ctags-directory (file-name-directory
                                                  (citre-tags-file-path)))))
          (zerolee--get-project-root))))
    (call-process-shell-command zerolee-ctags-command nil nil nil)))

(defun zerolee--update-ctags ()
  "更新 ctags 文件."
  (when (> (time-convert (time-since zerolee-timestamp) 'integer) 180)
    (zerolee-regenerate-ctags)
    (setq-local zerolee-timestamp (current-time))))

(defun zerolee--tags-config (&optional create)
  "用来配置代码变量、函数的跳转."
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)
  (when (or (citre-tags-file-path) create)
    (add-hook 'xref-backend-functions #'citre-xref-backend nil t)
    (add-hook 'completion-at-point-functions
              #'citre-completion-at-point -100 t)
    (add-hook 'after-save-hook #'zerolee--update-ctags nil t)
    (setq-local imenu-create-index-function
                #'citre-imenu-create-index-function))
  (when (eq major-mode 'js-mode)
    (define-key js-mode-map (kbd "M-.") #'xref-find-definitions)))

(add-hook 'prog-mode-hook
          (lambda ()
            (when (not (derived-mode-p 'lisp-data-mode))
              (zerolee--tags-config))
            (setq forward-sexp-function nil)))

(add-hook 'sgml-mode-hook #'zerolee--tags-config)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lua-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lua-mode
  :bind (:map lua-mode-map
              ("C-M-x" . lua-send-defun)
              ("C-x C-e" . lua-send-dwim)
              ("C-c I" . lua-inspect))
  :config
  (defun lua-send-dwim ()
    (interactive)
    (if (region-active-p)
        (call-interactively #'lua-send-region)
      (call-interactively #'lua-send-current-line)))
  (defun lua-inspect-helper (value)
    (with-current-buffer lua-process-buffer
      (save-excursion
        (search-backward ">" nil nil 2)
        (let ((symbol (buffer-substring-no-properties
                       (+ (point) 2) (- (point-max) 3))))
          (delete-region (+ (point) 2) (point-max))
          (if (string= symbol "table")
              (lua-send-string
               (format "print('');for k,v in pairs(%s) do print(k, v)end"
                       value))
            (lua-send-string (format "print('');print(%s)" value)))))))
  (defun lua-inspect ()
    (interactive)
    (let ((value (thing-at-point 'symbol t)))
      (lua-send-string (format "=type(%s)" value))
      (run-at-time 0.005 nil #'lua-inspect-helper value))))

(provide 'init-programs)
;;; init-programs.el ends here
