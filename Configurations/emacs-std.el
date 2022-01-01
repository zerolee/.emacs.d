;;; emacs-std.el --- Emacs 的一些基础配置-*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(setq display-time-default-load-average nil)
(setq display-time-mail-string "")
(display-time)
;; 关掉开机信息
(setq inhibit-startup-message t)

;;; mode-line 上取消 eldoc 显示
(setq eldoc-minor-mode-string "")

(setq auto-revert-mode-text "")

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
;;(setq show-paren-delay 0)
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

;;; narrow
(put 'narrow-to-region 'disabled nil)

(delete-selection-mode t)

(electric-pair-mode 1)

;;; org
(setq org-agenda-files
      '("~/note/plan"  "~/note/emacs"))
(setq org-html-htmlize-output-type nil)

(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))

;; 设置环境变量
(setenv "EMACS_START" "emacs_start")
(setenv "FZF_DEFAULT_COMMAND" "fd --type file")

;; Using MELPA
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;;; 配置字体
(push '(font . "Sarasa Fixed SC") default-frame-alist)
(set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)

;;; 配置 xref-search-program
(setq xref-search-program 'ripgrep)

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
                  "^\\*Backtrace\\*$"
                  "^\\*Ibuffer\\*$"
                  "^\\*.*Shell Command.*\\*$"
                  "^\\*e?shell\\*"
                  "^\\*Messages\\*$"
                  "^\\*ansi-term\\*"
                  "^\\*Javascript REPL"
                  "^\\*sly-mrepl"))
  (push `(,buffer
          (display-buffer-reuse-window
           display-buffer-in-side-window)
          (reusable-frames . visible)
          (side            . bottom)
          (window-height   . 0.33))
        display-buffer-alist))

;;; 配置窗口永远在右方，占据 45% 大小
(dolist (buffer '("^\\*lsp-help\\*$"
                  "^\\*eglot-help"
                  "^\\*sly-description\\*$"
                  "^\\*vc-"
                  ".el.gz$"
                  "^*eldoc"))
  (push `(,buffer
          (display-buffer-reuse-window
           display-buffer-in-side-window)
          (reusable-frames . visible)
          (side            . right)
          (window-width   . 0.45))
        display-buffer-alist))
;;; 备份文件
(setq backup-directory-alist '(("\\.*$" . "~/tmp/emacs_backup_file")))

;;; 开关输入法
(defun zerolee-ime-enable ()
  "启用输入法."
  (call-process "fcitx5-remote" nil nil nil "-o"))
(defun zerolee-ime-disable ()
  "关闭输入法."
  (call-process "fcitx5-remote" nil nil nil "-c"))
(provide 'emacs-std)
;;; emacs-std.el ends here
