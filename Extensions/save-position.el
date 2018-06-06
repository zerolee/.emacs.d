;;; save-position.el --- 一个的保存 point 的实现 -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ivy)


(defvar sp-position-ring nil
  "这是用来保存 markpoint 的，理论上无限制.")

(defvar sp-ring-position-pointer nil
  "当前所在 pointer.")


(defun struct-point-equal (sp1 sp2)
  "比较两个 point 是否相等.

SP1 和 SP2 是由 `sp-position-info' 构造而成."
  (and (string-equal (cadr sp1) (cadr sp2))
       (equal (cddr sp2) (cddr sp1))))


(defun bury-dup-element-from-list (list element)
  "舍弃重复的元素.

将 ELEMENT 从 LIST 中移除开去."
  (if list
      (if (struct-point-equal (car list) element)
          (cdr list)
        (cons (car list)
              (bury-dup-element-from-list (cdr list) element)))
    nil))

(defun sp-goto-buffer (buffer)
  "去所在 buffer.

BUFFER 即所要去的地方."
  (let ((list (window-list)))
    (while list
      (if (not (string-equal buffer (buffer-name)))
          (other-window 1))
      (setq list (cdr list)))
    (switch-to-buffer (get-buffer buffer))))



(defun sp-context-mark ()
  "当前位置相关信息.

展示信息：字符串，为了给 ivy 提供显示使用
位置信息：(buffer-name . name)"
  (let ((current-point (point))
        (current-line (progn
                        (beginning-of-line)
                        (1+ (count-lines 1 (point)))))
        (beg (progn
               (beginning-of-line)
               (point)))
        context-string)
    (end-of-line)
    (setq context-string (buffer-substring beg (point)))
    (goto-char current-point)
    (concat (buffer-name)
            ":"
            (let ((str (number-to-string current-line)))
              (put-text-property 0 (length str) 'face 'font-lock-keyword-face str) str)
            ": " context-string)))

(defun sp-position-info ()
  "位置信息."
  (cons (sp-context-mark)
        (cons (buffer-name) (point-marker))))


(defun index-compute (arg length)
  "计算 mark ring 下标.

ARG 为指针移动的次数， LENGTH 为 mark ring 的长度."
  (let* ((ring-pointer-length (length sp-ring-position-pointer))
         (return-value (+ arg (- length ring-pointer-length))))
    (if (< return-value 0)
        (+ return-value length)
      return-value)))
;;; 将 sp-ring-position-pointer 移动向 sp-position-ring 的下 arg 个元素
(defun rotate-mark-ring-pointer (arg)
  "Rotate the mark point in the mark ring.

ARG 为指针移动的次数"
  "interactive p"
  (let ((length (length sp-position-ring)))
    (if (zerop length)
        (error "Mark point ring is empty")
      (setq sp-ring-position-pointer
            (nthcdr (% (index-compute arg length)
                       length)
                    sp-position-ring)))))


(defun sp-push-position-to-ring ()
  "将当前位置的 markpoint 存储入 ring， 如果当前位置已经存储过，则从 ring 中删除."
  (interactive)
  (let ((pointer
         (bury-dup-element-from-list sp-position-ring (sp-position-info))))
    (if (= (length pointer) (length sp-position-ring))
        (progn
          (setq sp-position-ring (cons (sp-position-info) sp-position-ring))
          (setq sp-ring-position-pointer sp-position-ring)
          (message "添加当前位置的 point"))
      (progn
        (setq sp-position-ring pointer)
        (message "移除当前所在位置的 markpoint")))))

(defun sp-get-position-from-ring (&optional arg)
  "得到 sp-ring-position-pointer 所指向的 ring 同时，将其往后移动一次.

ARG 为指针移动的次数"
  (interactive "P")
  (if (zerop (length sp-position-ring))
      (error "POSITION-RING 为空，请先 MARK")
    (progn
      (if (eq arg '-)
          (rotate-mark-ring-pointer -2)
        (if (struct-point-equal (car sp-ring-position-pointer)
                                (sp-position-info))
            (rotate-mark-ring-pointer 1)))
      (if (get-buffer (cadar sp-ring-position-pointer))
          (progn
            (sp-goto-buffer (cadar sp-ring-position-pointer))
            (goto-char (marker-position (cddar sp-ring-position-pointer)))
            (rotate-mark-ring-pointer 1))
        (setq sp-position-ring
              (bury-dup-element-from-list sp-position-ring
                                          (car  sp-ring-position-pointer)))
        (rotate-mark-ring-pointer 1)
        (sp-get-position-from-ring)))))

(defun sp-show-all-position-in-ring ()
  "显示所有被标记的位置信息."
  (interactive)
  (ivy-read "mark ring: " sp-position-ring
            :action '(lambda (x)
                       (if (zerop (length sp-position-ring))
                           (error "POSITION-RING 为空，请先 MARK")
                         (sp-goto-buffer (cadr x))
                         (goto-char (marker-position (cddr x)))))))
(provide 'save-position)
;;; save-position.el ends here
