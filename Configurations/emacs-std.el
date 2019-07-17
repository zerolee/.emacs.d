;;; -*- lexical-binding: t; -*-
(setq display-time-default-load-average nil)
(display-time)
;; 关掉开机信息
(setq inhibit-startup-message t)

;;; mode-line 上取消 eldoc 显示
(setq eldoc-minor-mode-string "")

(setq column-number-mode t)


;; hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        try-expand-list
        try-expand-all-abbrevs
        try-expand-dabbrev-all-buffers
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-dabbrev-from-kill))


;; 在重名buffer前面加上其父目录的名字
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;Also highlight parens,
(show-paren-mode t)
;;showing matching parentheses
;;(setq show-paren-delay 0
(setq show-paren-style 'parenthesis)
;; showing region between parentheses
;;(setq show-paren-style 'expression)


;;;edit Compressed File
(require 'jka-compr)


;;;display and open image
(auto-image-file-mode)

;;; C-x C-l downcase-region: 文本块全部改为小写，该命令默认是 disabled
;;; C-x C-u upcase-region: 文本块全部改为大写， 该命令默认是 disabled
;;; 启用该命令
(put 'upcase-region 'disabled 0)
(put 'downcase-region 'disabled 0)


(delete-selection-mode t)

(electric-pair-mode 1)

;;; org
(setq org-agenda-files
      '("~/note/plan"  "~/note/emacs"))
(setq org-html-htmlize-output-type nil)

(advice-add 'org-insert-heading-respect-content :after
            #'(lambda (&rest args)
                (ove-mode 0)))
(advice-add 'org-meta-return :after
            #'(lambda (&rest args)
                (ove-mode 0)))
(advice-add 'org-open-at-point :before
            #'(lambda (&rest args)
                (unless (and (boundp 'sp-position-ring)
                             (sp--position-same-pos))
                  (sp-push-position-to-ring))))
(add-hook 'org-mode-hook #'(lambda ()
                             (setq truncate-lines nil)))

;; 设置环境变量
(setenv "EMACS_START" "emacs_start")

;;; .cquery 导入
(advice-add 'find-file :after
            #'(lambda (&rest args)
                (let* ((name (and (buffer-file-name)
                                  (not (file-exists-p (buffer-file-name)))
                                  (file-name-extension
                                   (concat "arbitrary"
                                           (file-name-nondirectory
                                            (buffer-file-name))))))
                       (realname (and name (concat name "." name))))
                  (and realname
                       (member realname (directory-files "~/模板"))
                       (insert-file-contents (concat "~/模板/" realname))
                       (yas-expand-snippet (buffer-string)
                                           (point-min) (point-max))))))

;; Using MELPA
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;;; 配置 dired-x
(autoload 'dired-jump "dired-x")
;;; 设置隐藏模式下要隐藏的文件
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")

;;; view-file 启动由 ove-mode 而不是 view-mode
(advice-add 'view-mode :around
            #'(lambda (orig-func &rest args)
                (ove-mode 1)
                (when (or (equal major-mode 'markdown-mode)
                          (equal major-mode 'gfm-mode)
                          (equal major-mode 'org-mode))
                  (hugomd-preview))))

;;; 配置字体
(add-to-list 'default-frame-alist '(font . "Sarasa Mono SC"))

;;; xref-find-definitions
(advice-add 'xref-find-definitions :after
            #'(lambda (&rest args)
                (ove-mode 1)))

;;; 配置 frame title 显示一个文件名或者 buffer 名
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; 配置窗口永远在下方，占据 33% 大小
(dolist (buffer '("^\\*Flymake diagnostics"
                  "^\\*Flycheck errors\\*$"
                  "^\\*Compile"
                  "^\\*Completions\\*$"
                  "^\\*compilation\\*$"
                  "^\\*Async Shell Command\\*$"))
  (add-to-list 'display-buffer-alist
               `(,buffer
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.33))))

;;; 配置窗口永远在右方，占据 33% 大小
(dolist (buffer '("^\\*Help\\*$"
                  "^\\*lsp-help\\*$"
                  "^\\*eglot-help"))
  (add-to-list 'display-buffer-alist
               `(,buffer
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . right)
                 (window-width   . 0.45))))
;;; 备份文件
(add-to-list 'backup-directory-alist
             '("\\.*$" . "~/tmp/emacs_backup_file"))
