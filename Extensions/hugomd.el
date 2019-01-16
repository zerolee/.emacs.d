;;;  hugomd.el ---  用来实时预览 Markdown -*- lexical-binding: t; -*-

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

;;; 首先需要安装 hugo， 本插件依赖 hugo
;;; 原理：1. 进入相应的文件夹执行 hugo server -D 启动 hugo 服务器
;;;       2. 将相应的 Markdown 文件进行处理复制到 post 文件夹下
;;;       3. 打开 Google 浏览器，打开相应的网址
;;; Code:

;;; 获取文件名
(defvar hugomd-root "~/tmp/tmp-blog/" "hugo new site my-blog 中的 my-blog")
(defvar hugomd--filename nil)
(defvar hugomd--hugo-file nil "post 目录下的文件")
(defvar hugomd--hugo-dired nil "post 目录下的目录")

(defun hugomd--copy-picture ()
  ;; 复制图片相关
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\\(!\\\[[A-Za-z-0-9./_ ]*\\\](\\\([A-Za-z./-0-9_]*\\\))\\\|<img +src ?= ?\"\\\([A-Za-z0-9./_]*\\\)\"[-a-zA-Z0-9./_ =\"]*>\\\)"
            nil t)
      (let* ((ms (or (match-string 2) (match-string 3)))
             (picture
              (if (and (string-match (regexp-quote "./") ms)
                       (= 0 (string-match (regexp-quote "./") ms)))
                  (substring ms 2)
                ms))
             (directory (file-name-directory picture))
             (file (file-name-nondirectory picture))
             (path (concat hugomd--hugo-dired "/")))
        (if directory
            (progn
              (unless (file-exists-p (concat path directory))
                (and (file-exists-p directory)
                     (copy-directory directory path t)))
              (unless (file-exists-p (concat path directory file))
                (and (file-exists-p picture)
                     (copy-file picture (concat path directory)))))
          (unless (file-exists-p (concat path file))
            (and (file-exists-p picture)
                 (copy-file picture path t))))))))
;;; 清除用于预览的文件和图片
(defsubst hugomd--clear-file ()
  (if (file-exists-p hugomd--hugo-dired)
      (delete-directory hugomd--hugo-dired t))
  (if (file-exists-p hugomd--hugo-file)
      (delete-file hugomd--hugo-file)))

;;; 用于将 buffer 写入指定文件
(defsubst hugomd--write-file (&rest unused)
  (write-region nil nil hugomd--hugo-file))

;;;###autoload
(defun hugomd-preview ()
  "预览 Markdown"
  (interactive)
  (unless hugomd--filename
    (setq-local hugomd--filename
                (concat (format-time-string "%Y%m%d%H%M%S")
                        (file-name-nondirectory
                         (buffer-file-name))))
    (setq-local hugomd--hugo-file
                (concat hugomd-root "content/post/" hugomd--filename))
    (setq-local hugomd--hugo-dired (substring hugomd--hugo-file 0 -3))
    (make-directory hugomd--hugo-dired))
  ;; 去相应目录下启动 hugo
  (let ((default-directory hugomd-root))
    (or (get-buffer "*hugo*")
        (make-process
         :name "hugo"
         :buffer "*hugo*"
         :command (list "hugo" "server" "-D")
         :noquery t)))

  ;; 复制文件
  (hugomd--write-file)

  ;; 复制图片
  (hugomd--copy-picture)

  ;; 将当前文件复制到相应的位置上
  (add-hook 'after-save-hook #'hugomd--write-file t t)
  (add-hook 'after-save-hook #'hugomd--copy-picture t t)

  (add-hook 'kill-buffer-hook #'hugomd--clear-file t t)

  ;; 打开浏览器
  (sleep-for 0.3)
  (browse-url
   (concat "http://localhost:1313/post/"
           (substring hugomd--filename 0 -3))))

(provide 'hugomd)
;;; hugomd.el ends here
