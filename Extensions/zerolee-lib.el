;;;  zerolee-lib.el ---  自用函数库 -*- lexical-binding: t; -*-

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

;;; 一些自己使用的函数
;;; Code:

(require 'cl-lib)
(defun zerolee-goto-some-window (buffer-or-name)
  "去一个指定 BUFFER-OR-NAME 的 window."
  (let ((window (get-buffer-window buffer-or-name)))
    (when window
      (select-window window))))

(defun zerolee-position-some-window (buffer)
  "查看 BUFFER 在打开的 window 中的位置，不存在返回 nil."
  (cl-position buffer (mapcar #'window-buffer (window-list))))

(defun zerolee-goto-ibuffer ()
  "打开并跳转到 Ibuffer."
  (interactive)
  (progn
    (ibuffer-list-buffers)
    (zerolee-goto-some-window "*Ibuffer*")))

(defun zerolee-time-to-seconds (times)
  "将由冒号分割的字符串形式的 TIMES 转换为整数形式的秒."
  (let ((seconds 0))
    (dolist (time (mapcar #'string-to-number (split-string times ":")) seconds)
      (setq seconds (+ time (* seconds 60))))))

(defun zerolee-time-duration (start-time end-time)
  "计算 END-TIME - START-TIME, 结果为一个整数(秒)."
  (- (zerolee-time-to-seconds end-time) (zerolee-time-to-seconds start-time)))

(defun zerolee-time-format (seconds)
  "将 SECONDS 转换成 HH:MM:SS 的格式."
  (let ((min (/ seconds 60)))
    (format "%02d:%02d:%02d" (/ min 60) (% min 60) (% seconds 60))))

(provide 'zerolee-lib)
;;; zerolee-lib.el ends here
