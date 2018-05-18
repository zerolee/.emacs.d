(display-time)
;; 关掉开机信息
(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

;; 设置环境变量
(setenv "EMACS_START" "emacs_start")

;;; .cquery 导入
(add-hook 'before-save-hook
          '(lambda ()
             (if (string-equal (file-name-nondirectory (buffer-file-name)) ".cquery")
                 (unless (file-exists-p (buffer-file-name))
                   (insert-file-contents "~/模板/.cquery")))))

;; Using MELPA
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;;; 配置 dired-x
(autoload 'dired-jump "dired-x")
;;; 设置隐藏模式下要隐藏的文件
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
