;;;  my-emms.el ---  配置 emms -*- lexical-binding: t; -*-

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
(require 'zerolee-lib)
(require 'ivy)

(defvar zerolee--emms-favourite "~/.emacs.d/emms/favourite/" "存放 playlist 的地方")
(use-package emms
  :config
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

  ;; setup
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
  (when (fboundp 'emms-cache)           ; work around compiler warning
    (emms-cache 1))
  (setq emms-player-list '(emms-player-mpv))

  (if (file-directory-p "~/音乐")
      (setq emms-source-file-default-directory "~/音乐")
    (setq emms-source-file-default-directory "~/Music"))
  (setq emms-playlist-buffer-name
        (concat " *" (file-name-base emms-source-file-default-directory) "*"))
  (setq emms-lyrics-dir "~/.lyrics")
  (emms-lyrics 1)
  (add-hook 'emms-player-started-hook 'emms-show)
  (emms-playing-time 1)
  (setq emms-lyrics-display-on-minibuffer t)
  (setq emms-lyrics-display-on-modeline nil)
  (setq emms-source-file-directory-tree-function
        'emms-source-file-directory-tree-find)
  (setq emms-repeat-playlist t)

  (define-key emms-playlist-mode-map (kbd ".") #'emms-toggle-repeat-track)
  (define-key emms-playlist-mode-map (kbd "SPC") #'emms-pause)
  (define-key emms-playlist-mode-map (kbd "v") #'scroll-up)
  (define-key emms-playlist-mode-map (kbd "<right>") #'emms-seek-forward)
  (define-key emms-playlist-mode-map (kbd "<left>") #'emms-seek-backward)
  (define-key emms-playlist-mode-map (kbd "b") #'scroll-down-line)
  (define-key emms-playlist-mode-map (kbd "f") #'scroll-up-line)
  (define-key emms-playlist-mode-map (kbd "m") #'emms-show)
  (define-key emms-playlist-mode-map (kbd "L") #'emms-playlist-set-playlist-buffer)
  (define-key emms-playlist-mode-map (kbd "l")
    #'(lambda ()
        (interactive)
        (emms-playlist-set-playlist-buffer (current-buffer))
        (message "%s 为当前播放列表" (buffer-name))))
  (define-key emms-playlist-mode-map (kbd ",")
    #'(lambda (time)
        "跳转到指定时间，格式 min:seconds，min 可省略"
        (interactive "s[min:]seconds: ")
        (let ((p (string-match ":" time))
              seconds)
          (if p
              (setq seconds (+ (* 60 (string-to-number (substring time 0 p)))
                               (string-to-number (substring time (1+ p)))))
            (setq seconds (string-to-number time)))
          (emms-ensure-player-playing-p)
          (emms-player-seek-to seconds))))
  (define-key emms-playlist-mode-map (kbd "M-p")
    #'(lambda ()
        (interactive)
        (emms-playlist-mode-previous 1)
        (setq emms-playlist-buffer-name (buffer-name))))
  (define-key emms-playlist-mode-map (kbd "M-n")
    #'(lambda ()
        (interactive)
        (emms-playlist-mode-next 1)
        (setq emms-playlist-buffer-name (buffer-name))))
  (define-key emms-playlist-mode-map (kbd "i")
    #'(lambda ()
        "去往正在播放的列表"
        (interactive)
        (switch-to-buffer emms-playlist-buffer)
        (setq emms-playlist-buffer-name (buffer-name))))
  (define-key emms-playlist-mode-map (kbd "R")
    #'(lambda ()
        "重新载入列表"
        (interactive)
        (emms-playlist-clear)
        (let ((emms-playlist-buffer (current-buffer)))
          (dolist (favourite (cddr (directory-files zerolee--emms-favourite)))
            (when (string-match (substring (buffer-name) 2 -1) favourite)
              (emms-add-playlist (concat zerolee--emms-favourite favourite))))
          (when (string-match (substring (buffer-name) 2 -1)
                              emms-source-file-default-directory)
            (emms-add-directory-tree emms-source-file-default-directory)))))
  (define-key emms-playlist-mode-map (kbd "C-x C-s")
    #'(lambda ()
        (interactive)
        (let ((emms-source-file-default-directory
               zerolee--emms-favourite))
          (call-interactively #'emms-playlist-save))))

  ;; 如何显示 track
  (setq emms-track-description-function
        #'(lambda (track)
            (let ((type (emms-track-type track)))
              (concat "♪ "
                      (cond ((eq 'file type)
                             (file-relative-name
                              (emms-track-name track)
                              emms-source-file-default-directory))
                            ((eq 'url type)
                             (emms-format-url-track-name (emms-track-name track)))
                            (t (concat (symbol-name type)
                                       ": " (emms-track-name track)))))))))

(defsubst zerolee--emms-toggle-popup ()
  "开关 emms popup"
  (let ((buffer (get-buffer emms-playlist-buffer-name)))
    (if (zerolee-position-some-window buffer)
        (zerolee-delete-some-window buffer)
      (emms-playlist-mode-go-popup)
      (emms-playlist-mode-center-current)
      (setq emms-playlist-buffer-name (buffer-name)))))

(defun zerolee-emms-default ()
  (interactive)
  (unless (get-buffer emms-playlist-buffer-name)
    (emms-play-directory-tree emms-source-file-default-directory))
  (zerolee--emms-toggle-popup))

(defun zerolee-emms-favourite ()
  (interactive)
  (when (file-exists-p zerolee--emms-favourite)
    (ivy-read "select favourite playlist: " (cddr (directory-files zerolee--emms-favourite))
              :action '(lambda (x)
                         (if (zerolee-position-some-window (get-buffer emms-playlist-buffer-name))
                             (progn
                               (zerolee-goto-some-window (get-buffer emms-playlist-buffer-name))
                               (setq emms-playlist-buffer-name
                                     (concat " *" (file-name-base x) "*"))
                               (if (get-buffer emms-playlist-buffer-name)
                                   (progn (switch-to-buffer (get-buffer emms-playlist-buffer-name))
                                          (emms-playlist-mode-play-current-track))
                                 (emms-playlist-new emms-playlist-buffer-name)
                                 (emms-playlist-set-playlist-buffer (get-buffer emms-playlist-buffer-name))
                                 (emms-play-playlist (concat zerolee--emms-favourite x))
                                 (emms-playlist-mode-go)
                                 (emms-playlist-mode-center-current)))
                           (setq emms-playlist-buffer-name
                                 (concat " *" (file-name-base x) "*"))
                           (if (get-buffer emms-playlist-buffer-name)
                               (progn (emms-playlist-set-playlist-buffer (get-buffer emms-playlist-buffer-name))
                                      (zerolee--emms-toggle-popup)
                                      (emms-playlist-mode-play-current-track))
                             (emms-playlist-new emms-playlist-buffer-name)
                             (emms-playlist-set-playlist-buffer (get-buffer emms-playlist-buffer-name))
                             (emms-play-playlist (concat zerolee--emms-favourite x))
                             (zerolee--emms-toggle-popup)))))))
