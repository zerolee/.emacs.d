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
(defvar zerolee--emms-history-buffer " *emms-history*"
  "存放播放历史的 buffer")
(defvar zerolee--emms-switched-buffer nil
  "存储切换记录的变量")
(defvar zerolee--emms-loop-point-A nil
  "A-B loop points 的起始时间")
(defvar zerolee--emms-loop-point-B nil
  "A-B loop points 的终止时间")
(defvar zerolee--emms-loop-point-A-B-timer nil
  "A-B loop points 相关定时器")


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
(define-key emms-playlist-mode-map (kbd "=") #'emms-volume-raise)
(define-key emms-playlist-mode-map (kbd "b") #'scroll-down-line)
(define-key emms-playlist-mode-map (kbd "f") #'scroll-up-line)
(define-key emms-playlist-mode-map (kbd "j") #'next-line)
(define-key emms-playlist-mode-map (kbd "k") #'previous-line)
(define-key emms-playlist-mode-map (kbd "m") #'emms-show)
(define-key emms-playlist-mode-map (kbd "S") #'emms-sort)
(define-key emms-playlist-mode-map (kbd "L")
  #'(lambda ()
      "选择当前播放列表"
      (interactive)
      (ivy-read "选择一个列表为当前列表：" (mapcar #'buffer-name emms-playlist-buffers)
                :action '(lambda (x)
                           (emms-playlist-set-playlist-buffer x)
                           (message "%s 为当前播放列表" x)))))
(define-key emms-playlist-mode-map (kbd "B")
  #'(lambda ()
      "选择已经打开播放列表"
      (interactive)
      (ivy-read "选择一个已经打开播放列表：" (mapcar #'buffer-name emms-playlist-buffers)
                :action '(lambda (x)
                           (switch-to-buffer x)
                           (emms-playlist-mode-play-current-track)
                           (setq emms-playlist-buffer-name x)
                           (when (string= x zerolee--emms-history-buffer)
                             (emms-uniq))))))
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
      (if (eq emms-playlist-buffer (current-buffer))
          (when zerolee--emms-switched-buffer
            (switch-to-buffer zerolee--emms-switched-buffer))
        (setq zerolee--emms-switched-buffer (current-buffer))
        (switch-to-buffer emms-playlist-buffer))
      (setq emms-playlist-buffer-name (buffer-name))))
(define-key emms-playlist-mode-map (kbd "I")
  #'(lambda ()
      "去往正在播放的列表"
      (interactive)
      (switch-to-buffer emms-playlist-buffer)
      (setq emms-playlist-buffer-name (buffer-name))))
(define-key emms-playlist-mode-map (kbd "o")
  #'(lambda ()
      "Set/clear A-B loop points.

         执行第一次时设置变量 `zerolee--emms-loop-point-A'
         执行第二次时重复设置一个定时器，以便 track 可以在 A-B 之间循环
         执行第三次时取消定时器，清空变量"
      (interactive)
      (if zerolee--emms-loop-point-A
          (if zerolee--emms-loop-point-A-B-timer
              (progn
                (cancel-timer zerolee--emms-loop-point-A-B-timer)
                (setq zerolee--emms-loop-point-A nil)
                (setq zerolee--emms-loop-point-B nil)
                (setq zerolee--emms-loop-point-A-B-timer nil))
            (setq zerolee--emms-loop-point-B emms-playing-time)
            (setq zerolee--emms-loop-point-A-B-timer
                  (run-with-timer 0
                                  (- emms-playing-time zerolee--emms-loop-point-A)
                                  #'emms-seek-to zerolee--emms-loop-point-A)))
        (setq zerolee--emms-loop-point-A emms-playing-time))))
(define-key emms-playlist-mode-map (kbd "O")
  #'(lambda ()
      "剪切 A-B 之间的 track

         设置了 A-B loop points 后会调用 ffmpeg 剪切 A-B 之间的 track
         若是在只设定了 A 的情况下剪切 track 会清空 `zerolee--emms-loop-point-A'"
      (interactive)
      (when zerolee--emms-loop-point-A
        ;; 获取当前节点时间然后，然后调用剪辑，清除 `zerolee--emms-loop-point-A'
        ;; 和 `zerolee--emms-loop-point-B'
        ;; 剪辑成功后暂停当前正在运行的程序，然后打开相应的 track
        (unless zerolee--emms-loop-point-B
          (setq zerolee--emms-loop-point-B emms-playing-time))
        (let* ((track-name (emms-track-get (emms-playlist-track-at) 'name))
               (track-name-ext (file-name-extension track-name))
               command)
          (if (member track-name-ext '("mp3" "MP3" "wma" "flac" "ape" "aac"))
              (setq command (list "ffmpeg" "-ss" (number-to-string zerolee--emms-loop-point-A)
                                  "-t" (number-to-string (- zerolee--emms-loop-point-B zerolee--emms-loop-point-A))
                                  "-i" track-name "-acodec" "copy"
                                  (concat "/tmp/" (number-to-string zerolee--emms-loop-point-A)
                                          "." track-name-ext)))
            (setq command (list "ffmpeg" "-ss" (number-to-string zerolee--emms-loop-point-A)
                                "-t" (number-to-string (- zerolee--emms-loop-point-B zerolee--emms-loop-point-A))
                                "-i" track-name "-c:v" "libx264" "-c:a" "aac"
                                "-strict" "experimental" "-b:a" "98k"
                                (concat "/tmp/" (number-to-string zerolee--emms-loop-point-A)
                                        "." track-name-ext))))
          (make-process
           :name "ffmpeg"
           :buffer "*ffmpeg*"
           :command command
           :noquery t
           :sentinel (lambda (proc _)
                       (when (eq 'exit (process-status proc))
                         (shell-command (concat "mpv /tmp/" (number-to-string zerolee--emms-loop-point-A)
                                                "." track-name-ext))))))
        (unless zerolee--emms-loop-point-A-B-timer
          (setq zerolee--emms-loop-point-B nil)))))
(define-key emms-playlist-mode-map (kbd "g")
  #'(lambda ()
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
(define-key emms-playlist-mode-map (kbd "C-x C-s")
  #'(lambda ()
      (interactive)
      (let ((emms-source-file-default-directory
             zerolee--emms-favourite))
        (call-interactively #'emms-playlist-save))))
(define-key emms-playlist-mode-map (kbd "d")
  #'(lambda ()
      "Visit the track at point in a `dired' buffer."
      (interactive)
      (let ((track (emms-playlist-track-at)))
        (if track
            (let ((name (emms-track-get track 'name))
                  (type (emms-track-get track 'type)))
              (cond ((eq type 'file)
                     (dired (file-name-directory name))
                     (goto-char 0)
                     (search-forward (file-name-base name) nil t 1)
                     (dired-move-to-filename))
                    ((eq type 'url)
                     (let ((sbf (substring (buffer-name) 2 -1)))
                       (dolist (favourite (cddr (directory-files zerolee--emms-favourite)))
                         (when (string-match sbf favourite)
                           (find-file (concat zerolee--emms-favourite favourite))
                           (ove-mode 1)
                           (search-forward name nil t 1)
                           (beginning-of-line)))))
                    (t (error "Can't visit this track type in Dired"))))
          (error "No track at point")))))

;; 如何显示 track
(setq emms-track-description-function
      #'(lambda (track)
          (let ((type (emms-track-type track)))
            (concat "♪ "
                    (cond ((eq 'file type)
                           (file-name-base (emms-track-name track)))
                          ((eq 'url type)
                           (file-name-base
                            (substring (file-name-directory
                                        (emms-format-url-track-name
                                         (emms-track-name track)))
                                       0 -1)))
                          (t (concat (symbol-name type)
                                     ": " (emms-track-name track))))))))
(emms-playlist-new zerolee--emms-history-buffer)

(advice-add 'emms-player-start :after
            #'(lambda (&rest _)
                "每当开始播放一首歌的时候便将这首歌保存入播放列表"
                (with-current-buffer emms-playlist-buffer
                  (save-excursion
                    (emms-playlist-mode-center-current)
                    (kill-ring-save (point-at-bol) (point-at-eol))))
                (with-current-buffer zerolee--emms-history-buffer
                  (save-excursion
                    (goto-char 0)
                    (emms-playlist-mode-yank)))))

(defsubst zerolee--emms-toggle-popup ()
  "开关 emms popup"
  (let ((buffer (get-buffer emms-playlist-buffer-name)))
    (if (zerolee-position-some-window buffer)
        (zerolee-delete-some-window buffer)
      (emms-playlist-mode-go-popup)
      (emms-playlist-mode-center-current)
      (setq emms-playlist-buffer-name (buffer-name)))))

;;;###autoload
(defun zerolee-emms-default ()
  (interactive)
  (unless (get-buffer emms-playlist-buffer-name)
    (emms-play-directory-tree emms-source-file-default-directory))
  (zerolee--emms-toggle-popup))

;;;###autoload
(defun zerolee-emms-favourite ()
  (interactive)
  (when (file-exists-p zerolee--emms-favourite)
    (ivy-read "select favourite playlist: "
              (cddr (directory-files zerolee--emms-favourite))
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
                             (zerolee--emms-toggle-popup))))
              :initial-input "m3u")))

(provide 'my-emms)
;;; my-emms.el ends here
