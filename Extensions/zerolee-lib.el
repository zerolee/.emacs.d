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

;;; 一些自己使用的函数、宏……
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

(cl-defun zerolee-time-to-seconds (times &aux (seconds 0))
  "将由冒号分割的字符串形式的 TIMES 转换为整数形式的秒."
  (dolist (time (mapcar #'string-to-number (split-string times ":")) seconds)
    (setq seconds (+ time (* seconds 60)))))

(defun zerolee-time-duration (start-time end-time)
  "计算 END-TIME - START-TIME, 结果为一个整数(秒)."
  (- (zerolee-time-to-seconds end-time) (zerolee-time-to-seconds start-time)))

(cl-defun zerolee-time-format (seconds &aux (min (/ seconds 60)))
  "将 SECONDS 转换成 HH:MM:SS 的格式."
  (format "%02d:%02d:%02d" (/ min 60) (% min 60) (% seconds 60)))


;;; 便捷的快捷键绑定
(defun zerolee-slots->set-key (keymaps slot)
  "KEYMAPS 是一个由 list(返回 keymap 的函数) 或者 symbol(keymap) 组成的 lists.
SLOT 应该是一个包含两个元素的(快捷键 绑定到的值) list.
\(zerolee-slots->set-key '(flymake-mode-keymap) '(\"C-\\\\\" nil))
→((define-key flymake-mode-keymap (kbd \"C-\\\\\") nil));
\(zerolee-slots->set-key '(flymake-mode-keymap) '([key-chord ?d ?f] nil))
→((define-key flymake-mode-keymap [key-chord 100 102] nil))"
  (cl-loop for keymap in keymaps
           append
           (cl-loop with command = (car (last slot))
                    for key in (butlast slot)
                    if (stringp key)
                    collect `(define-key ,keymap (kbd ,key) ,command)
                    else
                    collect `(define-key ,keymap ,key ,command))))

(defmacro zerolee-set-key (&rest slots)
  "SLOTS 为 keymap 或者按键绑定.
绑定快捷键的便捷宏，示例：
\(zerolee-set-key (current-global-map)
 (\"\\[isearch-forward-regexp]\" #'isearch-forward-regexp)
 (\"\\[isearch-backward-regexp]\" #'isearch-backward-regexp))"
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


;;; 模拟 Common Lisp 的 with-open-file，相关函数请局限在 with-open-file 中使用.
(defvar-local cl-current-position (point-min) "当前位置.")

(defun cl--read-helper (sexp-content new-pos eof-error-p eof)
  "一个 cl-read 相关的辅助函数.
SEXP-CONTENT: 获取的内容所用的 sexp.
NEW-POS: 每一次读取完数据后新的位置.
EOF-ERROR-P: 出错了应该如何处理，直接报错还是返回 EOF 的值."
  (if (<= cl-current-position (point-max))
      (prog2
          (goto-char cl-current-position)
          (eval sexp-content)
        (setq-local cl-current-position (eval new-pos)))
    (if eof-error-p
        (error "End of file!")
      eof)))

(cl-defun cl-read-line (&optional _buf (eof-error-p t) eof)
  "读取一行.
BUF: 读取所在的 buffer，EOF-ERROR-P 出错了应该如何处理，直接报错还是返回 EOF."
  (cl--read-helper '(buffer-substring-no-properties (point-at-bol) (point-at-eol))
                   '(1+ (point-at-eol)) eof-error-p eof))

(cl-defun cl-read-char (&optional _buf (eof-error-p t) eof)
  "读取一个字符.
BUF: 读取所在的 buffer，EOF-ERROR-P 出错应该如何处理，直接报错还是返回 EOF."
  (or (cl--read-helper '(char-after) '(1+ (point)) eof-error-p eof)
      (cl--read-helper '(char-after) '(1+ (point)) eof-error-p eof)))

(defun cl-write-line (string &optional _buf)
  "写入一行并换行.
STRING 为写入的字符串，BUF 为要写入的 buffer。"
  (goto-char cl-current-position)
  (insert (format "%s\n" string))
  (setq-local cl-current-position (point-max)))

(defun cl-write-string (string &optional _buf)
  "写入一个字符串.
STRING 为写入的字符串，BUF 为要写入的 buffer."
  (goto-char cl-current-position)
  (insert string)
  (setq-local cl-current-position (point-max)))

(defalias 'cl-write-char 'cl-write-string "写入一个字符.")
(defalias 'cl-write-byte 'cl-write-string "写入一个字节.")
(defalias 'cl-read-byte 'cl-read-char "读取一个字节.")

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
  "每个元素由 (cl . el) 组成，他们分别代表 Common Lisp 的控制字符和 Emacs Lisp.
的控制字符，暂不支持循环以及条件选择等复杂功能.")

(defun cl-parse-format (control-string)
  "格式化字符串，CONTROL-STRING 为需要分析的字符串."
  (dolist (cs cl-format-alist control-string)
    (setq control-string
          (string-replace (car cs) (cdr cs) control-string))))

(defun cl-format (destination control-string &rest format-arguments)
  "一个模拟 Common Lisp format 的函数.
DESTINATION 格式化字符串写入的地方，CONTROL-STRING 为需要分析的字符串.
FORMAT-ARGUMENTS 为格式化参数."
  (cond ((eq destination t)
         (apply #'message (cl-parse-format control-string) format-arguments))
        ((eq destination nil)
         (apply #'format (cl-parse-format control-string) format-arguments))
        (t (with-current-buffer destination
             (insert (apply #'format (cl-parse-format control-string)
                            format-arguments))))))

(cl-defun cl-file-position (_buf &optional (position 0 position-p))
  "只有一个参数 BUF 时返回文件中的当前位置.
\(已被读取或者写入流的元素数量，否则将流的位置设置到该描述的位置上).
POSITION 的值为 :start :end 或者一个非负整数."
  (if (not position-p)
      (1- cl-current-position)
    (setq-local cl-current-position
                (cond ((eq position :start) (point-min))
                      ((eq position :end) (point-max))
                      (t (1+ position))))
    t))

(cl-defmacro with-open-file ((str filename &key (direction :input)
                                  (element-type 'base-char)
                                  (if-exists :supersede)
                                  if-does-not-exist)
                             &body body)
  "模拟 Common Lisp 版 `with-open-file' 的一个宏.
STR 中保存的是操作的 buffer.
FILENAME：打开的文件(写入或者读取)
DIRECTION: 写入文件还是读取文件(默认值：:input 读取)，:output 为写入，
ELEMENT-TYPE：默认值 'base-char 以字符形式读写文件，其他值则为单字节.
IF-EXISTS： 默认值 :supersede 文件存在则覆盖，设置为 :append，文件存在则追加.
IF-DOES-NOT-EXIST：文件不存在报错，设置为 :create，则文件不存在时自动创建文件.
BODY：用户代码书写区域."
  (declare (indent defun))
  (if (eq direction :output)
      `(with-temp-file ,filename
         (let ((,str (current-buffer)))
           ,@(unless (eq element-type 'base-char)
               `((set-buffer-multibyte nil)))
           ,@(when (eq if-does-not-exist :create)
               `((unless (file-exists-p ,filename)
                   (make-empty-file ,filename))))
           ,@(when (eq if-exists :append)
               `((insert-file-contents ,filename)
                 (setq-local cl-current-position (point-max))))
           ,@body))
    `(with-temp-buffer
       (let ((,str (current-buffer)))
         ,@(unless (eq element-type 'base-char)
             `((set-buffer-multibyte nil)))
         (insert-file-contents ,filename)
         ,@body))))


;; (with-open-file (in "/tmp/hello.world"
;;                     :direction :input)
;;   (message "%s" (cl-read-line in t :eof))
;;   (message "%s" (cl-read-line))
;;   (message "%s" (cl-read-line in nil :eof))
;;   (message "%s" (cl-read-line in nil :eof))
;;   (message "%s" (cl-read-line in nil :eof))
;;   (message "%s" (cl-read-line in nil))
;;   (message "%s" (cl-read-line in nil :eof))
;;   (message "%s" in))

;; (with-open-file (output "/tmp/hello.world"
;;                         :direction :output)
;;   (cl-write-line "hello, world")
;;   (cl-write-line "look, good")
;;   (print "(+ 1 2)" output))

(defun cl-complement (fun)
  "返回一个返回值和 FUN 的返回值相反的函数."
  (lambda (&rest x)
    (not (apply fun x))))

(defun cl-byte (size position)
  "返回一个被其他函数(如：`cl-ldb')使用的byte specifier.
SIZE：需要解出（或设置）的位数量.
POSITION: 最右边那位相对整数中最不重要位来说以零开始的位置.
\(cl-byte 8 0) => (8 . 0)."
  (cons size position))

(defun cl-ldb (bytespec integer)
  "加载字节(load byte): 从一个整数中解出和设置(setf)任意数量的连续位.
函数接受一个字节描述符(BYTESPEC)和需要解出位数据的那个整数(INTEGER).
返回由解出的位代表的整数.
\(cl-ldb (cl-byte 8 0) #xabcd)=> 205 (#xcd).
\(cl-ldb (cl-byte 8 8) #xabcd)=> 171 (#xab)."
  (logand (ash integer (- (cdr bytespec)))
          (- (expt 2 (car bytespec)) 1)))

(defun cl-dpb (newbyte bytespec integer)
  "存储字节(deposit byte): 替换整数中的连续位.
将 INTEGER 中 BYTESPEC 所描述的位置设置为 NEWBYTE，并返回，NEWBYTE 是右对齐的.
\(cl-dpb 128 (cl-byte 8 0) 0) ; => 128 (#x80)
\(cl-dpb 1 (cl-byte 1 10) 0) ; => 1024 (11 bits, #x400)
\(cl-dpb -2 (cl-byte 2 10) 0) ; => 2048 (12 bits, #x800)
\(cl-dpb 1 (cl-byte 2 10) 2048) ; => 1024 (11 bits, #x400)"
  (let ((mask (- (expt 2 (car bytespec)) 1)))
    (logior (ash (logand mask newbyte) (cdr bytespec))
            (logand integer (lognot (ash mask (cdr bytespec)))))))

;;; 为 cl-ldb 定义 setter method
(gv-define-expander cl-ldb              ;NAME
  (lambda (do bytespec place)           ;HANDLER
    (gv-letplace (getter setter) place
      (funcall do `(cl-ldb ,bytespec ,getter) ;获取 PLACE 值的表达式
               (lambda (v);接受表达式 v，返回一个新表达式，将 PLACE 设置为 v
                 `(progn
                    ,(funcall setter `(cl-dpb ,v ,bytespec ,getter))
                    ,v))))))

(provide 'zerolee-lib)
;;; zerolee-lib.el ends here
