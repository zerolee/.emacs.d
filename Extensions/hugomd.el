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
(defvar hugomd-dired "~/tmp/tmp-blog/")
(defvar hugomd--filename nil)
(defvar hugomd--hugo-file nil)

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
             (path (concat (substring hugomd--hugo-file 0 -3) "/")))
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
                (concat hugomd-dired "content/post/" hugomd--filename))
    (make-directory (substring hugomd--hugo-file 0 -3)))
  ;; 去相应目录下启动 hugo
  ;; (setq display-buffer-alist
  ;;       '(("\\*Async Shell Command\\*"  (display-buffer-no-window))))

  ;; (or (get-buffer "*Async Shell Command*")
  ;;     (async-shell-command (concat "cd " hugomd-dired " && hugo server -D")))
  (let ((default-directory hugomd-dired))
    (or (get-buffer "*hugo*")
        (start-process "hugo" "*hugo*" "hugo" "server" " -D")))

  ;; 文件不存在则复制模板文件
  (let ((filename (buffer-file-name)))
    (unless (file-exists-p filename)
      (setq filename "~/模板/md.md"))
    (copy-file filename
               hugomd--hugo-file t))


  ;; 复制图片
  (hugomd--copy-picture)

  ;; 将当前文件复制到相应的位置上
  (add-hook 'after-save-hook
            '(lambda ()
               (copy-file (buffer-file-name)
                          hugomd--hugo-file t))
            t t)
  (add-hook 'after-save-hook #'hugomd--copy-picture t t)

  ;; 打开浏览器
  (browse-url
   (concat "http://localhost:1313/post/"
           (substring hugomd--filename 0 -3))))

(provide 'hugomd)
;;; hugomd.el ends here
