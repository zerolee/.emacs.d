;;; emacs-std.el --- Emacs 的一些基础配置-*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; mode-line 设置
(setq display-time-default-load-average nil
      display-time-mail-string ""
      eldoc-minor-mode-string ""
      auto-revert-mode-text "")

(display-time)
;; 关掉开机信息
(setq inhibit-startup-message t)

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
      '("~/note/plan"  "~/note/emacs")
      org-html-htmlize-output-type nil
      org-adapt-indentation t)          ;org 回车换行

(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))

;; 设置环境变量
(setenv "EMACS_START" "emacs_start")
(setenv "FZF_DEFAULT_COMMAND" "fd --type file")

;; Using MELPA
(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;;; 配置字体
(push '(font . "Sarasa Fixed SC") default-frame-alist)

;;; 配置 project, xref-search-program
(defconst project-discover-files
  '(".project" "Makefile" "CMakeLists.txt" "go.mod"  "tox.ini" "manage.py"
    "build.gradle" "WORKSPACE" "composer.json" "rebar.config" "Cargo.toml"
    "Gruntfile.js" "gulpfile.js" "package.json" "angular.json" "SConstruct"
    "requirements.txt" ".angular-cli.json" "application.properties" "pom.xml"
    "gradlew" ".bloop" "build.sc" "setup.py" "meson.build" "dune-project"
    "build.sbt" "project.clj" "poetry.lock" "Gemfile" "shard.yml" "mix.exs"
    ".midje.clj" "build.boot" "deps.edn" "DESCRIPTION" "stack.yaml" "Cask"
    "info.rkt" "pubspec.yaml" "Pipfile")
  "有以上这些文件的话就是根目录.")

(defun my/project-try-local (dir)
  "返回一个 list, 1.项目类型，2.项目定位文件，3.为根目录."
  (cl-loop for f in project-discover-files
           when (locate-dominating-file dir f)
           return (list 'local f it)))
(setq project-find-functions '(project-try-vc my/project-try-local))
(cl-defmethod project-root ((project (head local)))
  (cl-third project))

(defun my/project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -H -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))
(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'my/project-files-in-directory
          (or dirs (list (project-root project)))))


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
                  "^\\*lua*"
                  "^\\*sly-mrepl"))
  (push `(,buffer
          (display-buffer-reuse-window
           display-buffer-in-side-window)
          (reusable-frames . visible)
          (side            . bottom)
          (window-height   . 0.33))
        display-buffer-alist))

;;; 配置窗口永远在右方，占据 45% 大小
(dolist (buffer '("^\\*eglot-help"
                  "^\\*sly-description\\*$"
                  "^\\*vc-"
                  ".el.gz$"
                  "^*eldoc"
                  "^\\*sly-inspector for"))
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


;;; remember
(use-package remember
  :ensure nil
  :custom
  (remember-data-file "~/.emacs.d/notes.org")
  :bind ("<C-f5>" . remember))

(provide 'emacs-std)
;;; emacs-std.el ends here
