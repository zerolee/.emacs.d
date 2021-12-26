;;;  init-tools.el ---  一些小工具合集 -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; 主要针对对 Emacs 自带的 [e]shell、ansi-term 进行配置
;;; 此外，此处也搜集了一些方便使用的小工具函数
;;; Code:
(eval-when-compile (require 'subr-x))

(defvar zerolee--eshell-path-hashtable (make-hash-table :test #'equal)
  "每次启动 eshell 的时候将启动时的路径存储进 hash-table 中.
格式为 (path . (eshell-num . term-buffer)).")

(defconst zerolee--compile-alist
  '((emacs-lisp-mode    . (emacs-lisp-byte-compile))
    (html-mode          . (browse-url-of-buffer))
    (nxhtml-mode        . (browse-url-of-buffer))
    (html-helper-mode   . (browse-url-of-buffer))
    (octave-mode        . (run-octave))
    ("\\.c\\'"          . "gcc -O2 %f -lm -o %n")
    ("\\.[Cc]+[Pp]*\\'" . "g++ -O2 %f -lm -o %n")
    ("\\.[Ss]\\'"       . "as  %f")
    ("\\.cs\\'"         . "mcs %f")
    ("\\.cron\\(tab\\)?\\'" . "crontab %f")
    ("\\.cu\\'"         . "nvcc %f -o %n")
    ("\\.cuf\\'"        . "nvfortran -Mcuda -O2 %f -o %n")
    ("\\.[Ff]\\'"       . "gfortran %f -o %n")
    ("\\.[Ff]90\\'"     . "gfortran %f -o %n")
    ("\\.go\\'"         . "go run %f")
    ("\\.hs\\'"         . "ghc %f -o %n")
    ("\\.java\\'"       . "javac -Xlint:deprecation -Xlint:fallthrough %f")
    ("\\.jl\\'"         . "julia %f")
    ("\\.js\\'"         . "js %f")
    ("\\.lua\\'"        . "lua %f")
    ("\\.m\\'"          . "gcc -O2 %f -lobjc -lpthread -o %n")
    ("\\.mp\\'"         . "mptopdf %f")
    ("\\.php\\'"        . "php %f")
    ("\\.pl\\'"         . "perl %f");;"perl -cw %f" ; syntax check
    ("\\.p[l]?6\\'"     . "perl6 %f")
    ("\\.py\\'"         . "python3 %f")
    ("\\.raku\\'"       . "perl6 %f")
    ("\\.rb\\'"         . "ruby %f")   ;; "ruby -cw %f" ; syntax check
    ("\\.rs\\'"         . "rustc %f -o %n")
    ("\\.swift\\'"      . "swiftc %f -o %n")
    ("\\.tex\\'"        . (tex-file))
    ("\\.texi\\'"       . "makeinfo %f"))
  "每个元素由 (REGEXP . STRING) or (MAJOR-MODE . STRING) 构成.
当文件名与 REGEXP 匹配 或者 `major-mode' 与 MAJOR-MODE 匹配时，
执行 STRING 中内容 %f 被文件名替换， %n 被无后缀文件名替换.")

(defconst zerolee--build-system-alist
  '(("Makefile"    . "make ")
    ("makefile"    . "make ")
    ("Gemfile"     . "bundle install")
    ("Rakefile"    . "rake ")
    ("Cargo.toml"  . "cargo build ")
    ("pants"       . "./pants %f")))

(puthash "max" 0 zerolee--eshell-path-hashtable)

(defsubst zerolee--get-java-package-name ()
  (pcase (split-string
          (save-excursion
            (goto-char (point-min))
            (buffer-substring-no-properties (point-min) (line-end-position))))
    (`(,(pred (string= "package")) ,java-package-name . ,_)
     (replace-regexp-in-string "[.;]" "/" java-package-name))))

(defun zerolee--get-code-info (N)
  "根据 N 获取该代码生成的程序后缀0、包名1、程序执行时长相2."
  (nth N (pcase major-mode
           ('java-mode `(".class" ,(zerolee--get-java-package-name) "java "))
           ('c-mode `("" "" "./"))
           ('js-mode `("" "scripts/" ""))
           ('css-mode `("" "styles/" "")))))

(defun zerolee--get-project-root ()
  "获取关联项目的 root."
  (require 'projectile)
  (expand-file-name
   (or (projectile-project-root)
       (string-trim-right default-directory (zerolee--get-code-info 1)))))

(defsubst zerolee--get-run-app ()
  "获取需要运行的程序，如果所需要运行的程序不存在返回 nil."
  (let* ((program
          (if (region-active-p)
              (buffer-substring-no-properties
               (region-beginning) (region-end))
            (file-name-base (buffer-file-name))))
         (app
          (if (file-exists-p
               (concat (file-name-directory (buffer-file-name)) program
                       (zerolee--get-code-info 0)))
              (concat (zerolee--get-code-info 1) program))))
    (when app
      (concat (zerolee--get-code-info 2) app))))

(defsubst zerolee--ansi-term (app et)
  "运行 APP, ET 为 Eshell num 与 `ansi-term' buffer."
  (require 'term)
  (if (cdr et)
      (pop-to-buffer-same-window (get-buffer (cdr et)))
    (if et
        (setcdr et (generate-new-buffer-name "*ansi-term*"))
      (setq et (cons nil (generate-new-buffer-name "*ansi-term*"))))
    (puthash default-directory et zerolee--eshell-path-hashtable))
  (term-send-string (pop-to-buffer-same-window
                     (term-ansi-make-term (cdr et) "bash"))
                    app)
  (term-send-input)
  (set-process-sentinel
   (get-process (buffer-name))
   (lambda (proc _)
     (when (eq 'exit (process-status proc))
       (kill-buffer (process-buffer proc))
       (setcdr et nil)
       (puthash default-directory et zerolee--eshell-path-hashtable)))))

(defsubst zerolee--open-js-comint (&optional arg)
  "开启一个 js-repl, 参数为 4， 则重启该实例"
  (interactive "P")
  (require 'js-comint)
  (let ((window (selected-window))
        (buffer (get-buffer "*Javascript REPL*")))
    (if (= arg 4)
        (call-interactively #'js-comint-reset-repl)
      (if (and buffer (get-buffer-window buffer))
          (delete-windows-on buffer)
        (call-interactively #'run-js)))
    (select-window window)))

;;;###autoload
(defun zerolee-eshell (&optional num)
  "若处于 `eshell-mode' 或 `term-mode' 中则删除该窗口.
否则的话，获取【正确】的关联目录，关联目录是否关联 eshell;
已关联的话直接打开关联的 eshell，否则的话打开一个新的 eshell;
存在可运行的程序时，默认打开 `ansi-term' 运行;
NUM 为 2 强制打开 gnome-terminal;
NUM 为 3 强制启动 eshell;
NUM 为 4 强制当前目录打开 eshell."
  (interactive "p")
  (require 'eshell)
  (require 'esh-mode)
  (if (or (memq major-mode  '(term-mode eshell-mode))
          (derived-mode-p 'comint-mode))
      (delete-window)
    (let* ((default-directory
            (if (= 4 num)
                default-directory
              (zerolee--get-project-root)))
           (et (gethash default-directory zerolee--eshell-path-hashtable))
           (max (gethash "max" zerolee--eshell-path-hashtable))
           (app (when (buffer-file-name) (zerolee--get-run-app))))
      (cond ((eq major-mode 'js-mode) (zerolee--open-js-comint num))
            ((and app (= num 1)) (zerolee--ansi-term app et))
            ((= num 2) (call-process-shell-command "EMACS_START=nil gnome-terminal"))
            ((car et)
             (let ((buffer (get-buffer (format "*eshell*<%s>" (car et)))))
               (if (and buffer (get-buffer-window buffer))
                   (delete-windows-on buffer)
                 (eshell (car et)))))
            (t
             (if et
                 (setcar et (1+ max))
               (setq et (list (1+ max))))
             (puthash default-directory et zerolee--eshell-path-hashtable)
             (eshell (1+ max))
             (puthash "max" (1+ max) zerolee--eshell-path-hashtable))))))

(defsubst zerolee--format-compile (compiler)
  "替换 COMPILER 中的 %n, %f."
  (let ((base (concat (zerolee--get-code-info 1)
                      (file-name-base buffer-file-name)))
        (name (concat (zerolee--get-code-info 1)
                      (file-name-nondirectory buffer-file-name))))
    (string-replace "%n" base (string-replace "%f" name compiler))))

;;; 代码来源：https://github.com/zenitani/elisp/blob/master/smart-compile.el
;;;###autoload
(defun zerolee-compile ()
  "对 `compile' 的一个轻微的包装."
  (interactive)
  (let ((default-directory (zerolee--get-project-root))
        (name (buffer-file-name))
        (compiler nil))
    (cond ((and (local-variable-p 'compile-command) compile-command))
          ((catch 'build
             (dolist (alist zerolee--build-system-alist)
               (when (file-readable-p (car alist))
                 (set (make-local-variable 'compile-command)
                      (zerolee--format-compile (cdr alist)))
                 (throw 'build t)))))
          (name
           (if (catch 'done
                 (dolist (alist zerolee--compile-alist)
                   (when (or (and (symbolp (car alist))
                                  (eq (car alist) major-mode))
                             (and (stringp (car alist))
                                  (string-match (car alist) name)))
                     (setq compiler (cdr alist))
                     (throw 'done compiler))))
               (if (listp compiler)
                   (eval compiler)
                 (set (make-local-variable 'compile-command)
                      (zerolee--format-compile compiler)))
             (if (string= "#!" (buffer-substring-no-properties 1 3))
                 (set (make-local-variable 'compile-command) name)))))
    ;; compile
    (when (or (null compiler)
              (not (listp compiler)))
      (call-interactively 'compile))))

;;;###autoload
(defun zerolee-find-file (&optional N)
  "查找文件：N=1 且存在 .gitignore 时调用 `counsel-git' 否则调用 `counsel-fzf'."
  (interactive "p")
  (let ((default-directory (zerolee--get-project-root)))
    (if (and (file-readable-p ".gitignore") (= 1 N))
        (call-interactively #'counsel-git)
      (call-interactively #'counsel-fzf))))

;;;###autoload
(defun zerolee-rg (&optional initvalue)
  "给定默认目录以及初始值 INITVALUE 后对 `counsel-rg' 的一个轻微的包装."
  (interactive)
  (let ((default-directory (zerolee--get-project-root)))
    (counsel-rg initvalue)))

;;;###autoload
(defun zerolee-go ()
  "综合了 `projectile-find-file-dwim' `zerolee-rg' 等的跳转函数."
  (interactive)
  (require 'projectile)
  (require 'ffap)
  (if (save-excursion
        (search-backward "#include" (line-beginning-position) t))
      (condition-case nil
          (call-interactively #'projectile-find-file-dwim)
        (error (call-interactively #'ffap)))
    (if (nth 3 (syntax-ppss))
        (if (ffap-file-at-point)
            (find-file (ffap-file-at-point))
          (call-interactively #'ffap))
      (zerolee-rg (concat "\\b" (thing-at-point 'symbol t) "\\b"))))
  (vesie-mode 1))


;;; 来源 https://github.com/bbatsov/crux/blob/2e16b828910c9b8acba37e712d21b517d2cf78cc/crux.el#L152
;;; 使用外部程序打开相关文件
;;;###autoload
(defun zerolee-open-with (arg)
  "使用外部程序打开浏览的文件或者当前光标下的链接.
处于 dired mode 时, 打开当前光标下的文件;
若当前光标下存在链接，使用外部程序打开链接;
使用 prefix ARG 时指定使用的外部程序."
  (interactive "P")
  (let ((current-file-name
         (cond ((eq major-mode 'dired-mode) (dired-get-file-for-visit))
               ((help-at-pt-string)
                (pcase (cdr (split-string (help-at-pt-string) ":" t " "))
                  ((or `(,path) `(,(pred (string= "file")) ,path) `(,_ ,path ,_))
                   (expand-file-name path))
                  (`(,proto ,path) (concat proto ":" path))))
               (t (or (thing-at-point 'url) buffer-file-name))))
        (program (if arg
                     (read-shell-command "Open current file with: ")
                   "open")))
    (call-process program nil 0 nil current-file-name)))


;;;###autoload
(defun zerolee-delete-window ()
  "Delete dedicate 状态为 side 的窗口."
  (interactive)
  (require 'ace-window)
  (let (side)
    (dolist (window (window-list))
      (when (eq (window-dedicated-p window) 'side)
        (setq side t)
        (delete-window window)))
    (unless side
      (ace-delete-window))))

;;; goto-last-edit
(defvar-local zerolee-goto-edit-list nil "保存 buffer-undo-list 的一个 buffer 变量.")
(defvar-local zerolee-goto-insert-count nil
  "保存插入的数据的个数，格式为((car . cdr)).
car: 插入的最后一个值的 point, cdr：插入的值的数量.")
(defvar-local zerolee-goto-prev nil)
;;;###autoload
(defun zerolee-goto-last-edit ()
  "跳到最后一个 edit 处，再次调用则跳转到倒数第二个处，以此类推."
  (interactive)
  (require 'xref)
  (when (and buffer-undo-list
             (listp buffer-undo-list))
    (unless (equal last-command 'zerolee-goto-last-edit)
      (setq zerolee-goto-edit-list buffer-undo-list)
      (setq zerolee-goto-insert-count nil)
      (setq zerolee-goto-prev nil)
      (setq this-command 'zerolee-goto-last-edit))
    (goto-char
     (or (catch 'done
           (while zerolee-goto-edit-list
             (let ((entry (car zerolee-goto-edit-list))
                   (p 0))
               (setq zerolee-goto-edit-list (cdr zerolee-goto-edit-list))
               (when (and
                      (consp entry)
                      (car entry)
                      (not (eq (car entry) 't))
                      (not (eq (car entry) 'apply))
                      (not (stringp (car entry)))
                      (if (markerp (car entry))
                          (and (/= (or (marker-position (car entry))
                                       (point))
                                   (point))
                               (<= (or (marker-position (car entry))
                                       (point))
                                   (point-max)))
                        (and (/= (point) (abs (cdr entry)))
                             (<= (abs (cdr entry)) (point-max)))))
                 (xref-push-marker-stack)
                 (when (markerp (car entry))
                   (throw 'done (marker-position (car entry))))
                 (dolist (var zerolee-goto-insert-count)
                   (when (or
                          (eq (car var) 'true)
                          (< (car var) (abs (cdr entry))))
                     (setq p (+ p (abs (cdr var))))))
                 (if (and zerolee-goto-prev
                          (> zerolee-goto-prev (abs (cdr entry))))
                     (setcdr (car zerolee-goto-insert-count)
                             (+ (cdar zerolee-goto-insert-count)
                                (- (abs (cdr entry)) (abs (car entry)))))
                   (and
                    (consp zerolee-goto-insert-count)
                    (setcar (car zerolee-goto-insert-count) 'true))
                   (push (cons (abs (car entry))
                               (- (abs (cdr entry)) (abs (car entry))))
                         zerolee-goto-insert-count))
                 (setq zerolee-goto-prev (abs (cdr entry)))
                 (when (and (/= (point) (+ p (abs (cdr entry))))
                            (<= (+ p (abs (cdr entry))) (point-max)))
                   (throw 'done (+ p (abs (cdr entry)))))))))
         (progn
           (message "Arrived last edit.")
           (point))))))

(provide 'init-tools)
;;; init-tools.el ends here
