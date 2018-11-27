;;; save-position.el --- 一个的保存、删除和展示位置信息的工具 -*- lexical-binding: t; -*-

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

;; 使用 sp-push-position-to-ring     来保存或者删除一个位置
;; 使用 sp-get-position-from-ring    来跳转不同的位置
;; 使用 sp-show-all-position-in-ring 来查看所有的位置
;; 你可以将他们绑定到你喜欢的按键上
;; 新版本参考了 sams-lib.el

;;; Code:
(require 'ivy)


(defvar sp-position-ring nil
  "这是用来保存位置信息的，理论上无限制.")

(defun sp--goto-position (marker)
  "去正确的 buffer 中正确的位置."
  (let ((list (window-list))
        (buffer (marker-buffer marker)))
    (while list
      (unless (equal buffer (current-buffer))
        (other-window 1))
      (setq list (cdr list)))
    (switch-to-buffer buffer)
    (goto-char marker)))

(defun sp--clear-non-exist-buffer ()
  "清除已经被杀掉的 buffer"
  (dolist (var sp-position-ring)
    (unless (member (marker-buffer (cdr var)) (buffer-list))
      (setq sp-position-ring (remove var sp-position-ring)))))

(defun sp--context-mark ()
  "当前位置相关信息.

展示信息：字符串，为了给 ivy 提供显示使用
位置信息：(buffer-name . name)"
  (let ((current-line (number-to-string (1+ (count-lines 1 (point-at-bol)))))
        (context-string (buffer-substring (point-at-bol) (point-at-eol))))
    (put-text-property 0 (length current-line) 'face 'font-lock-keyword-face current-line)
    (concat (buffer-name)
            ":"
            current-line
            ": " context-string)))

(defsubst sp--position-info ()
  "位置信息."
  (cons (sp--context-mark)
        (point-marker)))

(defsubst sp--position-same-pos ()
  (and sp-position-ring
       (equal (point) (marker-position (cdar sp-position-ring)))
       (equal (current-buffer) (marker-buffer (cdar sp-position-ring)))))

;;;###autoload
(defun sp-push-position-to-ring ()
  "将当前位置的 MARKER 存储入 ring， 如果当前位置已经存储过，则从 ring 中删除."
  (interactive)
  (if (sp--position-same-pos)
      (progn
        (setq sp-position-ring (cdr sp-position-ring))
        (message "移除当前所在位置的 MARKER"))
    (setq sp-position-ring (cons (sp--position-info) sp-position-ring))
    (message "添加当前位置的 MARKER")))

;;;###autoload
(defun sp-get-position-from-ring (&optional num)
  (interactive "P")
  (if (null sp-position-ring)
      (error "POSITION-RING 为空，请先 MARK"))
  (setq num
        (if (null num) (if (sp--position-same-pos) 1 0)
          (prefix-numeric-value num)))
  (setq num (mod num (length sp-position-ring)))
  (let ((top nil))
    (while (> num 0)
      (setq top (cons (car sp-position-ring) top))
      (setq sp-position-ring (cdr sp-position-ring))
      (setq num (1- num)))
    (setq sp-position-ring (append sp-position-ring (nreverse top)))
    (if (marker-position (cdar sp-position-ring))
        (sp--goto-position (cdar sp-position-ring))
      (setq sp-position-ring (cdr sp-position-ring))
      (sp-get-position-from-ring 1))))

;;;###autoload
(defun sp-show-all-position-in-ring ()
  "显示所有被标记的位置信息."
  (interactive)
  (sp--clear-non-exist-buffer)
  (ivy-read "mark ring: " sp-position-ring
            :action '(lambda (x)
                       (if (null sp-position-ring)
                           (error "POSITION-RING 为空，请先 MARK")
                         (sp--goto-position (cdr x))))))
(provide 'save-position)
;;; save-position.el ends here
