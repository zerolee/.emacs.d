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
(require 'ox)
(require 'ox-md)

;;; 获取文件名
(defvar hugomd-root "~/tmp/tmp-blog/" "hugo new site my-blog 中的 my-blog.")
(defvar hugomd-blog-root "~/backups/website/my-blog/" "blog 存放目录.")
(defvar hugomd-base-url "https://zerolee.github.io/" "blog 主页地址.")
(defvar hugomd--filename nil)
(defvar hugomd--hugo-file nil "post 目录下的文件")
(defvar hugomd--hugo-dired nil "post 目录下的目录")
(defvar hugomd--hugo-files nil "本次 emacs 启动后 post 目录下的所有文件")
(defvar hugomd--hugo-direds nil "本次 emacs 启动后 post 目录下的所有目录")

(defun hugomd--copy-link-file ()
  "复制 markdown 文件中引用的图片"
  (if (equal major-mode 'org-mode)
      (let ((hugo-dired hugomd--hugo-dired))
        (with-current-buffer "*Org MD Export*"
          (let ((hugomd--hugo-dired hugo-dired))
            (hugomd--copy-link-file-real))))
    (hugomd--copy-link-file-real)))

(defun hugomd--copy-link-file-real ()
  "复制 markdown 文件中引用的图片"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\\(\\\[[()A-Za-z-0-9./_ ]*\\\](\\\([A-Za-z./-0-9_]*\\\)[-a-zA-Z0-9./_ =\"]*)\\\|<img +src ?= ?\"\\\([-A-Za-z0-9./_]*\\\)\"[-a-zA-Z0-9./_ =\"]*>\\\)"
            nil t)
      (let* ((ms (or (match-string 2) (match-string 3)))
             (picture
              (if (and (string-match (regexp-quote "./") ms)
                       (= 0 (string-match (regexp-quote "./") ms)))
                  (substring ms 2)
                ms))
             (directory (file-name-directory picture))
             (file (concat hugomd--hugo-dired picture)))
        (when directory
          (let ((hugomd--hugo-directory (concat hugomd--hugo-dired directory)))
            (unless (file-exists-p hugomd--hugo-directory)
              (make-directory hugomd--hugo-directory t))))
        (unless (file-exists-p file)
          (and (file-exists-p picture)
               (copy-file picture file t)))))))

(defsubst hugomd--clear-file ()
  "清除用于预览的文件和图片"
  (dolist (dired hugomd--hugo-direds)
    (if (file-exists-p dired)
        (delete-directory dired t)))
  (dolist (file hugomd--hugo-files)
    (if (file-exists-p file)
        (delete-file file))))

(defsubst hugomd--write-file ()
  "用于将 buffer 写入指定文件"
  (if (equal major-mode 'org-mode)
      (let ((org-export-show-temporary-export-buffer nil)
            (hugo-file hugomd--hugo-file))
        (org-md-export-as-markdown)
        (with-current-buffer "*Org MD Export*"
          (write-region nil nil hugo-file)))
    (write-region nil nil hugomd--hugo-file)))

;;;###autoload
(defun hugomd-preview ()
  "预览 Markdown"
  (interactive)
  (unless hugomd--filename
    (let ((file-name
           (if (equal major-mode 'org-mode)
               (concat (substring (buffer-name) 0 -3) "md")
             (buffer-name))))
      (setq-local hugomd--filename
                  (concat (format-time-string "%Y%m%d%H%M%S")
                          (file-name-nondirectory
                           file-name))))
    (setq-local hugomd--hugo-file
                (concat hugomd-root "content/post/" hugomd--filename)
                hugomd--hugo-dired
                (concat (substring hugomd--hugo-file 0 -3) "/"))
    (make-directory hugomd--hugo-dired)
    (push hugomd--hugo-file hugomd--hugo-files)
    (push hugomd--hugo-dired hugomd--hugo-direds))
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
  (hugomd--copy-link-file)

  ;; 将当前文件复制到相应的位置上
  (add-hook 'after-save-hook #'hugomd--write-file t t)
  (add-hook 'after-save-hook #'hugomd--copy-link-file t t)

  (add-hook 'kill-emacs-hook #'hugomd--clear-file)

  ;; 打开浏览器
  (sleep-for 0.3)
  (browse-url
   (concat "http://localhost:1313/post/"
           (substring hugomd--filename 0 -3))))

(defsubst hugomd--get-filename ()
  (completing-read
   "输入文章名: "
   (cl-loop for filename in (directory-files
                             (concat hugomd-blog-root "content/post/"))
            when (string-match "\\.md$" filename)
            collect (substring filename 0 -3))))

;;;###autoload
(defun hugomd-new-blog ()
  "打开或者新建一个 blog."
  (interactive)
  ;; 根据用户的输入去相应的目录下生成相应的文件名
  (let ((blogname (concat "post/" (hugomd--get-filename) ".md"))
        (default-directory hugomd-blog-root))
    (let ((where-blogname (concat "content/" blogname)))
      (unless (file-exists-p where-blogname)
        (call-process "hugo" nil nil  nil "new" blogname))
      (find-file (concat "content/" blogname))
      (call-interactively #'hugomd-preview)
      (search-forward "draft: "))))

;;;###autoload
(defun hugomd-deploy-blog ()
  "部署一个 blog."
  (interactive)
  (let ((default-directory hugomd-blog-root))
    (call-process "hugo" nil nil nil "--theme=light-hugo"
                  (concat "--baseUrl=" hugomd-base-url)))
  ;; push 到 github
  (let ((default-directory (concat hugomd-blog-root "public")))
    (call-interactively #'magit))
  ;; 打开相应的网址查看
  (browse-url hugomd-base-url))

(provide 'hugomd)
;;; hugomd.el ends here
