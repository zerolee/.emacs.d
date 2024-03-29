;;;  init-emms.el ---  配置 emms -*- lexical-binding: t; -*-

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

;;; 简介
;;; 个人配置 emms 的文件
;;; Code:
(require 'ivy)
(require 'zerolee-lib)

(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-player-simple)
(require 'emms-player-mpv)
(require 'emms-playlist-mode)
(require 'emms-cache)
(require 'emms-lyrics)
(require 'emms-playing-time)
(require 'emms-i18n)
(require 'emms-volume)

(defvar zerolee--emms-favourite "~/.emacs.d/emms/favourite/"
  "存放 playlist 的地方")
(defvar zerolee--emms-switched-buffer nil
  "存储切换记录的变量")
(defvar zerolee--emms-hash-pls (make-hash-table :test #'equal :size 130)
  "将 pls 文件里的 title 和 file 存储在 hash-table 中")

;; setup
(setq emms-playlist-default-major-mode 'emms-playlist-mode)
(setq emms-track-initialize-functions (cons 'emms-info-initialize-track nil))
(when (fboundp 'emms-cache)           ; work around compiler warning
  (emms-cache 1))
(setq emms-player-list '(emms-player-mpv))

(advice-add 'emms-source-playlist-pls-files :around
            (lambda (_orig-func &rest _)
              (let ((files nil))
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward "^File\\([0-9]*\\)=\\(.+\\)$" nil t)
                    (push (match-string 2) files)
                    (when (re-search-forward (format "^Title%s=\\(.+\\)$" (match-string 1)) nil t)
                      (puthash (car files) (match-string 1) zerolee--emms-hash-pls))))
                (nreverse files))))

(if (file-directory-p "~/音乐")
    (setq emms-source-file-default-directory "~/音乐")
  (setq emms-source-file-default-directory "~/Music"))
(setq emms-playlist-buffer-name
      (concat " *" (file-name-base emms-source-file-default-directory) "*"))
(setq emms-lyrics-dir "~/.lyrics")
(emms-lyrics 1)
(add-hook 'emms-player-started-hook 'emms-show)
(emms-playing-time-mode 1)
(setq emms-lyrics-display-on-minibuffer t)
(setq emms-lyrics-display-on-modeline nil)
(setq emms-source-file-directory-tree-function
      'emms-source-file-directory-tree-find)
(setq emms-repeat-playlist t)

(zerolee-set-key emms-playlist-mode-map
  ("." #'emms-toggle-repeat-track)
  ("SPC" #'emms-pause)
  ("v" #'scroll-up)
  ("<right>" #'emms-seek-forward)
  ("<left>" #'emms-seek-backward)
  ("=" #'emms-volume-raise)
  ("b" #'scroll-down-line)
  ("f" #'scroll-up-line)
  ("j" #'next-line)
  ("k" #'previous-line)
  ("m" #'emms-show)
  ("S" #'emms-sort)
  ("L" (lambda ()
         "选择当前播放列表"
         (interactive)
         (message "%s 为当前播放列表"
                  (emms-playlist-set-playlist-buffer
                   (completing-read "选择一个列表为当前列表："
                                    (mapcar #'buffer-name emms-playlist-buffers))))))
  ("B" (lambda ()
         "选择已经打开播放列表"
         (interactive)
         (setq emms-playlist-buffer-name
               (completing-read "选择一个已经打开播放列表："
                                (mapcar #'buffer-name emms-playlist-buffers)))
         (switch-to-buffer emms-playlist-buffer-name)
         (emms-playlist-mode-play-current-track)))
  ("l" (lambda ()
         (interactive)
         (emms-playlist-set-playlist-buffer (current-buffer))
         (message "%s 为当前播放列表" (buffer-name))))
  ("," (lambda (times)
         "跳转到指定时间，格式 min:seconds，min 可省略."
         (interactive "s[min:]seconds: ")
         (emms-ensure-player-playing-p)
         (emms-player-seek-to (zerolee-time-to-seconds times))))
  ("M-p" (lambda ()
           (interactive)
           (emms-playlist-mode-previous 1)
           (setq emms-playlist-buffer-name (buffer-name))))
  ("M-n" (lambda ()
           (interactive)
           (emms-playlist-mode-next 1)
           (setq emms-playlist-buffer-name (buffer-name))))
  ("i" (lambda ()
         "去往正在播放的列表"
         (interactive)
         (if (eq emms-playlist-buffer (current-buffer))
             (when zerolee--emms-switched-buffer
               (switch-to-buffer zerolee--emms-switched-buffer))
           (setq zerolee--emms-switched-buffer (current-buffer))
           (switch-to-buffer emms-playlist-buffer))
         (setq emms-playlist-buffer-name (buffer-name))))
  ("I" (lambda ()
         "去往正在播放的列表"
         (interactive)
         (switch-to-buffer emms-playlist-buffer)
         (setq emms-playlist-buffer-name (buffer-name))))
  ("R" (lambda ()
         "重新载入播放列表"
         (interactive)
         (emms-playlist-clear)
         (let ((emms-playlist-buffer (current-buffer)))
           (dolist (favourite (cddr (directory-files zerolee--emms-favourite)))
             (when (string-match (substring (buffer-name) 2 -1) favourite)
               (emms-add-playlist (concat zerolee--emms-favourite favourite))))
           (when (string-match (substring (buffer-name) 2 -1)
                               emms-source-file-default-directory)
             (emms-add-directory-tree emms-source-file-default-directory)))))
  ("C-x C-s" (lambda ()
               (interactive)
               (let ((emms-source-file-default-directory
                      zerolee--emms-favourite))
                 (call-interactively #'emms-playlist-save))))
  ("d" (lambda ()
         "Visit the track at point in a `dired' buffer."
         (interactive)
         (let ((track (emms-playlist-track-at)))
           (if track
               (let ((name (emms-track-get track 'name))
                     (type (emms-track-get track 'type)))
                 (cond ((eq type 'file)
                        (dired (file-name-directory name))
                        (goto-char (point-min))
                        (search-forward (file-name-base name) nil t 1)
                        (dired-move-to-filename))
                       ((eq type 'url)
                        (let ((sbf (substring (buffer-name) 2 -1)))
                          (dolist (favourite (cddr (directory-files zerolee--emms-favourite)))
                            (when (string-match sbf favourite)
                              (find-file (concat zerolee--emms-favourite favourite))
                              (vesie-mode 1)
                              (search-forward name nil t 1)
                              (beginning-of-line)))))
                       (t (error "Can't visit this track type in Dired"))))
             (error "No track at point"))))))

;; 如何显示 track
(setq emms-track-description-function
      (lambda (track)
        (let ((type (emms-track-type track))
              (name (emms-track-name track)))
          (concat "♪ "
                  (cond ((eq 'file type)
                         (file-name-base name))
                        ((eq 'url type)
                         (or (gethash (emms-format-url-track-name name)
                                      zerolee--emms-hash-pls)
                             (file-name-base
                              (substring (file-name-directory
                                          (emms-format-url-track-name
                                           name))
                                         0 -1))))
                        (t
                         (concat (symbol-name type) ": " name)))))))

(defsubst zerolee--emms-toggle-popup ()
  "开关 emms popup"
  (if (get-buffer-window emms-playlist-buffer-name)
      (delete-windows-on emms-playlist-buffer-name)
    (if (or (< (window-width) 50)
            (eq major-mode 'emms-playlist-mode))
        (emms-playlist-mode-go)
      (emms-playlist-mode-go-popup))
    (emms-playlist-mode-center-current)
    (setq emms-playlist-buffer-name (buffer-name))))

;;;###autoload
(defun zerolee-emms-default (&optional num)
  "num = 1 打开所有音乐， num = 2 指定文件夹打开， num = 4 打开歌单."
  (interactive "p")
  (cond ((= num 1)
         (unless (get-buffer emms-playlist-buffer-name)
           (emms-play-directory-tree emms-source-file-default-directory))
         (zerolee--emms-toggle-popup))
        ((= num 2)
         (call-interactively #'emms-play-directory-tree))
        ((= num 4)
         (zerolee-emms-favourite))))

(defun zerolee-emms-favourite ()
  (when (file-exists-p zerolee--emms-favourite)
    (let ((love (completing-read "select favourite playlist: "
                                 (cons emms-source-file-default-directory
                                       (cddr (directory-files zerolee--emms-favourite)))
                                 nil nil "m3u"))
          (window (get-buffer-window emms-playlist-buffer-name)))
      (setq emms-playlist-buffer-name
            (concat " *" (file-name-base love) "*"))
      (and window (select-window window))
      (if (get-buffer emms-playlist-buffer-name)
          (emms-playlist-set-playlist-buffer (get-buffer emms-playlist-buffer-name))
        (emms-playlist-new emms-playlist-buffer-name)
        (emms-playlist-set-playlist-buffer (get-buffer emms-playlist-buffer-name))
        (if (string-equal love emms-source-file-default-directory)
            (emms-add-directory-tree love)
          (emms-add-playlist (concat zerolee--emms-favourite love))))
      (unless (eq (current-buffer) emms-playlist-buffer)
        (zerolee--emms-toggle-popup))
      (emms-playlist-mode-play-current-track))))

(provide 'init-emms)
;;; init-emms.el ends here
