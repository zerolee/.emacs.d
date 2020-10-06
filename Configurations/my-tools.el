;;;  my-tools.el ---  shell 设置相关 -*- lexical-binding: t; -*-

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

;;; 主要对 Emacs 自带的 [e]shell进行配置
;;; Code:


(defvar zerolee--eshell-path-hashtable (make-hash-table :test #'equal)
  "每次启动 eshell 的时候将启动时的路径存储进 hash-table 中")

(puthash "max" 0 zerolee--eshell-path-hashtable)

(defsubst zerolee--eshell-get-java-package-content ()
  (let* ((first-line
          (save-excursion
            (goto-char (point-min))
            (buffer-substring-no-properties (point-min) (line-end-position))))
         (pos (string-match "\\<package\\>" first-line)))
    (if (and pos (<= pos 1))
        (replace-regexp-in-string
         "\\." "/"
         (string-trim (substring first-line (+ 8 pos) -1)))
      nil)))

(defun zerolee--eshell-get-project-root ()
  "获取关联项目的 root"
  (require 'projectile)
  (or (projectile-project-root)
      (if (equal major-mode 'java-mode)
          (let ((root (zerolee--eshell-get-java-package-content)))
            (when root
              (let ((pos (string-match root default-directory)))
                (substring default-directory 0 pos)))))
      default-directory))

(defsubst zerolee--eshell-get-app ()
  "获取需要运行的程序，如果所需要运行的程序不存在返回 nil"
  (let ((app
         (if (region-active-p)
             (let ((program
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))))
               (if (and (equal major-mode 'java-mode)
                        (file-exists-p
                         (concat
                          (file-name-directory (buffer-file-name)) program ".class")))
                   (concat
                    (file-name-directory
                     (zerolee--eshell-get-java-package-name))
                    program)))
           (if (or (and (equal major-mode 'java-mode)
                        (file-exists-p (replace-in-string ".java" ".class" (buffer-file-name))))
                   (and (buffer-file-name)
                        (file-exists-p (file-name-base (buffer-file-name)))))
               (car
                (split-string (zerolee--eshell-get-java-package-name) "\\."))))))
    (when app
      (if (equal major-mode 'java-mode)
          (concat "java " app)
        (concat "./" app)))))

(defun zerolee--eshell-get-java-package-name ()
  (let ((root (zerolee--eshell-get-java-package-content))
        (bfn (buffer-file-name)))
    (if root
        (let ((pos (string-match root bfn)))
          (substring bfn pos))
      (file-name-nondirectory (buffer-file-name)))))

(defsubst zerolee--run (num app)
  "当 num 为 1 时运行指定程序，否则打开一个新的 gnome-terminal"
  (let ((default-directory (zerolee--eshell-get-project-root))
        (height (round (* 0.45 (frame-height))))
        (width (round (+ 1 (frame-width))))
        (left (round (+ 10 (cadar (frame-geometry)))))
        (up (round (+ (cddar (frame-geometry)) (* 0.51 (frame-outer-height))))))
    (shell-command (concat
                    "EMACS_START=nil "
                    "gnome-terminal --working-directory="
                    default-directory
                    " --geometry="
                    (format "%sx%s+%s+%s" width height left up)
                    (when (= 1 num)
                      (concat " -x bash -c '" app ";read' "))
                    "&> /dev/null"))))

;;;###autoload
(defun zerolee-eshell (&optional num)
  "若处于 eshell-mode 中则删除该窗口，

   否则的话，获取【正确】的关联目录，关联目录是否关联 eshell，
   已关联的话直接打开关联的 eshell，否则的话打开一个新的 eshell。
   存在可运行的程序时，默认打开 gnome-terminal 运行
   传入参数 2 强制打开 gnome-terminal
   传入参数 3 强制打开 eshell
   传入参数 4 强制当前目录启动 eshell "
  (interactive "p")
  (require 'eshell)
  (require 'esh-mode)
  (if (eq major-mode 'eshell-mode)
      (delete-window)
    (let* ((default-directory
             (if (= 4 num)
                 default-directory
               (zerolee--eshell-get-project-root)))
           (wn (gethash default-directory zerolee--eshell-path-hashtable))
           (max (gethash "max" zerolee--eshell-path-hashtable))
           (app (zerolee--eshell-get-app)))
      (if (and
           (or app (= num 2))
           (< num 3))
          (zerolee--run num app)
        (if wn
            (let ((buffer
                   (get-buffer (concat "*eshell*<"
                                       (number-to-string wn) ">"))))
              (if (and buffer (get-buffer-window buffer))
                  (delete-windows-on buffer)
                (eshell wn)))
          (puthash default-directory (1+ max) zerolee--eshell-path-hashtable)
          (eshell (1+ max))
          (puthash "max" (1+ max) zerolee--eshell-path-hashtable))))))

;;;###autoload
(defun zerolee-compile (&optional arg)
  "对 `compile' 和 smart-compile 的一个轻微的包装"
  (interactive "p")
  (require 'smart-compile)
  (let ((default-directory (zerolee--eshell-get-project-root)))
    (if (not (equal major-mode 'java-mode))
        (smart-compile arg)
      (set (make-local-variable 'compile-command)
           (concat "javac " (zerolee--eshell-get-java-package-name)))
      (call-interactively #'compile))))

;;;###autoload
(defun zerolee-find-file ()
  "查找文件"
  (interactive)
  (let ((default-directory (zerolee--eshell-get-project-root)))
    (if (file-readable-p ".gitignore")
        (call-interactively #'counsel-git)
      (call-interactively #'counsel-fzf))))

;;;###autoload
(defun zerolee-rg (&optional initvalue)
  "查找文件"
  (interactive)
  (let ((default-directory (zerolee--eshell-get-project-root)))
    (counsel-rg initvalue)))

;;;###autoload
(defun zerolee-go ()
  "综合了 projectile-find-file-dwim zerolee-rg 等的跳转函数"
  (interactive)
  (require 'projectile)
  (let* ((filename (thing-at-point 'filename t))
         (suffix (nth 1 (split-string filename "\\."))))
    (when filename
      (if suffix
          (cond ((and (equal major-mode 'c-mode)
                      (or (string= "h" suffix)
                          (string= "c" suffix)))
                 (call-interactively #'projectile-find-file-dwim)))
        (zerolee-rg (concat "\\b" filename "\\b")))))
  (ove-mode 1))


;;; 来源 https://github.com/bbatsov/crux/blob/2e16b828910c9b8acba37e712d21b517d2cf78cc/crux.el#L152
;;; 使用外部程序打开相关文件
;;;###autoload
(defun zerolee-open-with (arg)
  "使用外部程序打开浏览的文件或者当前光标下的链接
处于 dired mode 时, 打开当前光标下的文件
处于 org mode 时，若当前光标下存在文件链接，使用外部程序打开链接文件
使用 prefix ARG 时指定使用的外部程序 "
  (interactive "P")
  (require 'dired)
  (require 'thingatpt)
  (let ((current-file-name
         (if (eq major-mode 'dired-mode)
             (dired-get-file-for-visit)
           (if (and
                (eq major-mode 'org-mode)
                (help-at-pt-string))
               (let ((current-string (split-string (help-at-pt-string) ":")))
                 (concat (if (string-match
                              (string-trim (nth 1 current-string))
                              "https")
                             (concat (string-trim (nth 1 current-string)) ":")
                           nil)
                         (expand-file-name
                          (string-trim
                           (car (last current-string 1))))))
             (or (thing-at-point 'url) buffer-file-name))))
        (program
         (if arg
             (read-shell-command "Open current file with: ")
           "xdg-open")))
    (call-process program nil 0 nil current-file-name)))


;;;###autoload
(defun zerolee-delete-window ()
  "delete dedicate 状态为 side 的窗口"
  (interactive)
  (require 'ace-window)
  (let (side)
    (dolist (window (window-list))
      (when (equal (window-dedicated-p window) 'side)
        (setq side t)
        (delete-window window)))
    (unless side
      (ace-delete-window))))

(provide 'my-tools)
;;; my-tools.el ends here
