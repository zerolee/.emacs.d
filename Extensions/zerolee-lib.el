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
(defun zerolee-goto-some-window (buffer)
  "去一个指定 buffer 的 window"
  (let ((position (zerolee-position-some-window buffer)))
    (when position
      (other-window position))))

(defun zerolee-delete-some-window (buffer)
  "删除一个指定 buffer 的 window"
  (let ((bf (current-buffer)))
    (zerolee-goto-some-window buffer)
    (when (equal buffer (current-buffer))
      (if (= (count-windows) 1)
          (bury-buffer)
        (delete-window))
      (zerolee-goto-some-window bf))))

(defun zerolee-position-some-window (buffer)
  "查看 buffer 在打开的 window 中的位置，不存在返回 nil"
  (cl-position buffer (mapcar #'window-buffer (window-list))))

(provide 'zerolee-lib)
;;; zerolee-lib.el ends here
