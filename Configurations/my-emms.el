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
