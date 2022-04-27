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


;;; 便捷的快捷键绑定
(defun zerolee-slots->set-key (keymaps slot)
  "(zerolee-slots->set-key '(flymake-mode-keymap) '(\"C-\\\\\" nil))
→(define-key flymake-mode-keymap (kbd \"C-\\\\\") nil);
(zerolee-slots->set-key '(flymake-mode-keymap) '([key-chord ?d ?f] nil))
→(define-key flymake-mode-keymap [key-chord 100 102] nil)"
  (cl-loop for keymap in keymaps
           append
           (cl-loop with command = (car (last slot))
                    for key in (butlast slot)
                    if (stringp key)
                    collect `(define-key ,keymap (kbd ,key) ,command)
                    else
                    collect `(define-key ,keymap ,key ,command))))

(defmacro zerolee-set-key (&rest slots)
  "绑定快捷键的便捷宏，示例：
(zerolee-set-key (current-global-map)
 (\"C-s\" #'isearch-forward-regexp)
 (\"C-r\" #'isearch-backward-regexp))"
  (declare (indent defun))
  `(progn
     ,@(cl-loop with keymaps = '((current-global-map))
                with last-is-keymap = nil
                for slot in slots
                if (or (atom slot) (and (consp slot) (= (length slot) 1)))
                do (if last-is-keymap
                       (push slot keymaps)
                     (setq keymaps (list slot)
                           last-is-keymap t))
                else
                append (progn
                         (setq last-is-keymap nil)
                         (zerolee-slots->set-key keymaps slot)))))


;;; 模拟 Common Lisp 的 with-open-file

(defvar-local current-position -1 "当前位置")

(defun cl-read-helper (sexp-content new-pos buf eof-error-p eof)
  (with-current-buffer buf
    (if (and (> current-position 0)
             (<= current-position (point-max)))
        (prog2
            (goto-char current-position)
            (eval sexp-content)
          (setq-local current-position (eval new-pos)))
      (if eof-error-p
          (error "end of file...")
        eof))))

(cl-defun cl-read-line (&optional (buf (current-buffer)) (eof-error-p t) eof)
  (cl-read-helper '(buffer-substring-no-properties (point-at-bol) (point-at-eol))
                  '(1+ (point-at-eol)) buf eof-error-p eof))

(cl-defun cl-read-char (&optional (buf (current-buffer)) (eof-error-p t) eof)
  (or (cl-read-helper '(char-after) '(1+ (point)) buf eof-error-p eof)
      (cl-read-helper '(char-after) '(1+ (point)) buf eof-error-p eof)))

(cl-defun cl-write-line (string &optional (buf (current-buffer)))
  (with-current-buffer buf
    (goto-char current-position)
    (insert (format "%s\n" string))
    (setq-local current-position (point-max))))

(cl-defun cl-write-string (string &optional (buf (current-buffer)))
  (with-current-buffer buf
    (goto-char current-position)
    (insert string)
    (setq-local current-position (point-max))))


(defconst cl-format-alist
  '(("~a" . "%s")
    ("~A" . "%s")
    ("~s" . "%S")
    ("~S" . "%S")
    ("~c" . "%c")
    ("~%" . "\n")
    ("~d" . "%d")
    ("~e" . "%e")
    ("~f" . "%f")
    ("~x" . "%x")
    ("~o" . "%o"))
  "每个元素由 (cl . el) 组成，他们分别代表 Common Lisp 的控制字符和 Emacs Lisp
的控制字符，暂不支持循环以及条件选择等复杂功能.")

(defun cl-parse-format (control-string)
  (dolist (cs cl-format-alist control-string)
    (setq control-string
          (string-replace (car cs) (cdr cs) control-string))))

(defun cl-format (destination control-string &rest format-arguments)
  "一个模拟 Common Lisp format 的函数."
  (cond ((eq destination t)
         (apply #'message (cl-parse-format control-string) format-arguments))
        ((eq destination nil)
         (apply #'format (cl-parse-format control-string) format-arguments))
        (t (with-current-buffer destination
             (insert (apply #'format (cl-parse-format control-string)
                            format-arguments))))))

(cl-defun cl-file-position (buf &optional (position 0 position-p))
  "只有一个参数 buf 时返回文件中的当前位置,
(已被读取或者写入流的元素数量，否则将流的位置设置到该描述的位置上).
position 的值为 :start :end 或者一个非负整数."
  (with-current-buffer buf
    (if (not position-p)
        (1- current-position)
      (setq-local current-position
                  (cond ((eq position :start) (point-min))
                        ((eq position :end) (point-max))
                        (t (1+ position))))
      t)))

(cl-defmacro with-open-file ((str filename &key (direction :input)
                                  (element-type 'base-char)
                                  (if-exists :supersede))
                             &body body)
  "模拟 Common Lisp 版 with-open-file 的一个宏，str 中保存的是操作的 buffer."
  (if (eq direction :output)
      `(with-temp-file ,filename
         (unwind-protect
             (let ((,str (current-buffer)))
               (if (eq ',element-type 'base-char)
                   (set-buffer-multibyte t)
                 (set-buffer-multibyte nil))
               (when (eq ,if-exists :append)
                 (insert-file-contents ,filename))
               (setq-local current-position (point-max))
               ,@body)
           (setq-local current-position -1)))
    `(with-temp-buffer
       (unwind-protect
           (let ((,str (current-buffer)))
             (if (eq ',element-type 'base-char)
                 (set-buffer-multibyte t)
               (set-buffer-multibyte nil))
             (setq-local current-position (point-min))
             (insert-file-contents ,filename)
             ,@body)
         (setq-local current-position -1)))))


;; (with-open-file (in "/tmp/hello.world"
;;                  :direction :input)
;;              (message "%s" (cl-read-line in t :eof))
;;              (message "%s" (cl-read-line))
;;              (message "%s" (cl-read-line in nil :eof))
;;              (message "%s" (cl-read-line in nil :eof))
;;              (message "%s" (cl-read-line in nil :eof))
;;              (message "%s" (cl-read-line in nil))
;;              (message "%s" (cl-read-line in nil :eof))
;;              (message "%s" in))

;; (with-open-file (output "/tmp/hello.world"
;;                      :direction :output)
;;              (cl-write-line "hello, world")
;;              (cl-write-line "look, good")
;;              (print "(+ 1 2)" output))

(defun cl-complement (fun)
  "以一个 fun 作为参数，它返回一个函数，这个函数的返回值总是和
fun 得到的返回值相反."
  (lambda (&rest x)
    (not (apply fun x))))

(provide 'zerolee-lib)
;;; zerolee-lib.el ends here
