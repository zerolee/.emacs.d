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
  (let ((buffer (get-buffer " *EMMS Playlist*")))
    (if (zerolee-position-some-window buffer)
        (zerolee-delete-some-window buffer)
      (emms-playlist-mode-go-popup)
      (emms-playlist-mode-center-current))))

(defun zerolee-emms-default ()
  (interactive)
  (unless (get-buffer " *EMMS Playlist*")
    (emms-play-directory-tree emms-source-file-default-directory))
  (zerolee--emms-toggle-popup))

(defun zerolee-emms-favourite ()
  (interactive)
  (let ((favourite "~/.emacs.d/emms/favourite"))
    (when (file-exists-p favourite)
      (ivy-read "select favourite playlist: " (cddr (directory-files favourite))
                :action '(lambda (x)
                           (emms-play-playlist (concat "~/.emacs.d/emms/favourite/" x))))))
  (zerolee-delete-some-window (get-buffer " *EMMS Playlist*"))
  (zerolee--emms-toggle-popup))
