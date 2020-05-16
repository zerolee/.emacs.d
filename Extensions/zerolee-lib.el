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

(require 'save-position)
(defun zerolee-goto-some-window (buffer)
  "去一个指定 buffer 的 window"
  (let ((list (window-list)))
    (while list
      (unless (equal buffer (current-buffer))
        (other-window 1))
      (setq list (cdr list)))))

(defun zerolee-delete-some-window (buffer)
  "删除一个指定 buffer 的 window"
  (if (equal buffer (current-buffer))
      (delete-window)
    (sp-push-position-to-ring)
    (zerolee-goto-some-window buffer)
    (when (equal buffer (current-buffer))
      (delete-window)
      (sp-get-position-from-ring)
      (sp-push-position-to-ring))))
(provide 'zerolee-lib)
;;; zerolee-lib.el ends here
