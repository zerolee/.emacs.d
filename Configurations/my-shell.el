;;;  my-shell.el ---  shell 设置相关 -*- lexical-binding: t; -*-

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

(require 'eshell)
(require 'smart-compile)
(require 'projectile)

(defvar zerolee--eshell-path-hashtable (make-hash-table :test #'equal)
  "每次启动 eshell 的时候将启动时的路径存储进 hash-table 中")

(puthash "max" 0 zerolee--eshell-path-hashtable)

(defsubst zerolee--eshell-get-java-package-content ()
  (let* ((first-line
          (save-excursion
            (goto-char (point-min))
            (buffer-substring-no-properties (point-min) (line-end-position))))
         (pos (string-match "package" first-line)))
    (if (and pos (<= pos 1))
        (replace-regexp-in-string
         "\\." "/"
         (string-trim (substring first-line (+ 8 pos) -1)))
      nil)))

(defun zerolee--eshell-get-java-package-root ()
  (let ((root (zerolee--eshell-get-java-package-content)))
    (if root
        (let ((pos (string-match root default-directory)))
          (substring default-directory 0 pos))
      nil)))

(defun zerolee--eshell-get-java-package-name ()
  (let ((root (zerolee--eshell-get-java-package-content))
        (bfn (buffer-file-name)))
    (if root
        (let ((pos (string-match root bfn)))
          (substring bfn pos))
      (file-name-nondirectory (buffer-file-name)))))


;;;###autoload
(defun zerolee-eshell (&optional num)
  "若处于 eshell-mode 中则删除该窗口

   否则，查看是否存在一个关联文件，若不存在则启动一个 eshell
   若存在则查看该文件是否与一个项目相关联，
   若是相关联则打开项目目录，并且启动 eshell，如果关联的 eshell 已经启动了则关闭
   否则，打开文件目录并启动 eshell，如果关联的 eshell 已经启动了则关闭
   传入参数 4 则强制打开文件所在目录并启动 eshell"
  (interactive "p")
  (cond ((eq major-mode 'eshell-mode) (delete-window))
        ((not (or (projectile-project-root)
                  (buffer-file-name)))
         (eshell))
        (t (let* ((fnd (or (projectile-project-root)
                           (and (equal major-mode 'java-mode)
                                (zerolee--eshell-get-java-package-root))
                           (file-name-directory (buffer-file-name))))
                  (wn (gethash fnd zerolee--eshell-path-hashtable))
                  (max (gethash "max" zerolee--eshell-path-hashtable))
                  (fnd2 (and (buffer-file-name)
                             (file-name-directory (buffer-file-name))))
                  (wn2 (gethash fnd2 zerolee--eshell-path-hashtable)))
             (when (= 4 num)
               (setq fnd fnd2)
               (setq wn wn2))
             (if wn
                 (let ((buffer
                        (get-buffer (concat "*eshell*<"
                                            (number-to-string wn) ">"))))
                   (if (zerolee-position-some-window buffer)
                       (delete-windows-on buffer)
                     (eshell wn)))
               (puthash fnd (1+ max) zerolee--eshell-path-hashtable)
               (let ((default-directory fnd)
                     (mj major-mode)
                     (name (zerolee--eshell-get-java-package-name)))
                 (eshell (1+ max))
                 (when (equal mj 'java-mode)
                   (insert (concat "java " (substring name 0 -5)))))
               (puthash "max" (1+ max) zerolee--eshell-path-hashtable))))))

;;; 之所以放在这里，是因为可以方便使用 zerolee--eshell-get-java-package-name 和
;;; zerolee--eshell-get-java-package-root 函数
;;;###autoload
(defun zerolee-compile ()
  "对 `compile' 和 smart-compile 的一个轻微的包装"
  (interactive)
  (cond ((projectile-project-root)
         (let ((default-directory (projectile-project-root)))
           (call-interactively #'smart-compile)))
        ((and (equal major-mode 'java-mode)
              (zerolee--eshell-get-java-package-root))
         (let ((default-directory (zerolee--eshell-get-java-package-root)))
           (set (make-local-variable 'compile-command)
                (concat "javac " (zerolee--eshell-get-java-package-name)))
           (call-interactively #'compile)))
        (t
         (call-interactively #'smart-compile))))

(provide 'my-shell)
;;; my-shell.el ends here
