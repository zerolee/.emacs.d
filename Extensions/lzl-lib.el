;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 一个的保存 point 的实现
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar lzl-point-ring nil
  "这是用来保存 mark 的，理论上无限制")

(defvar lzl-ring-mark-pointer nil
  "当前所在 pointer")

;;; 比较两个 point 是否相等
(defun struct-point-equal (sp1 sp2)
  (and (string-equal (cadr sp1) (cadr sp2))
       (equal (cddr sp2) (cddr sp1))))

;;; 舍弃重复的元素
(defun bury-dup-element-from-list (list element)
  (if list
      (if (struct-point-equal (car list) element)
	  (cdr list)
	(cons (car list)
	      (bury-dup-element-from-list (cdr list) element)))
    nil))

;;; 去所在 buffer
(defun lzl-goto-buffer (buffer)
  (let ((list (window-list)))
    (while list
      (if (not (string-equal buffer (buffer-name)))
	  (other-window 1))
      (setq list (cdr list)))
    (switch-to-buffer (get-buffer buffer))))


;;; 当前位置相关信息 (展示信息：字符串，为了给 ivy 提供显示使用
;;;                位置信息：(buffer-name . name))
(defun lzl-context-mark ()
  (let ((current-point (point))
	(current-line (progn
			(beginning-of-line)
			(1+ (count-lines 1 (point)))))
	(beg (point))
	context-string)
    (goto-char current-point)
    (forward-line 1)
    (end-of-line)
    (setq context-string (buffer-substring beg (point)))
    (goto-char current-point)
    (concat (buffer-name) " : " (number-to-string current-line) " : " context-string)))

(defun lzl-point-info ()
  (cons (lzl-context-mark)
	(cons (buffer-name) (point-marker))))


(defun index-compute (arg length)
  (let* ((ring-pointer-length (length lzl-ring-mark-pointer))
	 (return-value (+ arg (- length ring-pointer-length))))
    (if (< return-value 0)
	(+ return-value length)
      return-value)))
;;; 将 lzl-ring-mark-pointer 移动向 lzl-point-ring 的下 arg 个元素
(defun rotate-mark-ring-pointer (arg)
  "Rotate the mark point in the mark ring."
  "interactive p"
  (let ((length (length lzl-point-ring)))
    (if (zerop length)
	(error "Mark point ring is empty")

      (setq lzl-ring-mark-pointer
	    (nthcdr (% (index-compute arg length)
		       length)
		    lzl-point-ring)))))


(defun lzl-push-mark-to-ring ()
  "将当前位置的 point 存储入 ring， 如果当前位置已经存储过，则从 ring 中删除"
  (interactive)
  (let ((pointer
	 (bury-dup-element-from-list lzl-point-ring (lzl-point-info))))
    (if (= (length pointer) (length lzl-point-ring))
	(progn
	  (setq lzl-point-ring (cons (lzl-point-info) lzl-point-ring))
	  (rotate-mark-ring-pointer 0)
	  (message "添加当前位置的 point"))
      (progn
	(setq lzl-point-ring pointer)
	(rotate-mark-ring-pointer 1)
	(message "移除当前所在位置的 markpoint")))))

(defun lzl-get-mark-from-ring (&optional arg)
  "得到 lzl-pointer 所指向的 ring 同时，将其往后移动一次"
  (interactive "P")
  (if (zerop (length lzl-point-ring))
      (error "point-ring 为空，请先 mark")
    (progn
      (if (eq arg '-)
	  (rotate-mark-ring-pointer -2))
      (if (get-buffer (cadar lzl-ring-mark-pointer))
	  (progn
	    (lzl-goto-buffer (cadar lzl-ring-mark-pointer))
	    (goto-char (marker-position (cddar lzl-ring-mark-pointer)))
	    (rotate-mark-ring-pointer 1))
	(setq lzl-point-ring
	      (bury-dup-element-from-list lzl-point-ring
					  (car  lzl-ring-mark-pointer)))
	(rotate-mark-ring-pointer 1)
	(lzl-get-mark-from-ring)))))

(defun lzl-show-all-mark-in-ring ()
  (interactive)
  (ivy-read "mark ring: " lzl-point-ring
	    :action '(lambda (x)
		       (lzl-goto-buffer (cadr x))
		       (goto-char (marker-position (cddr x))))))
(provide 'lzl-lib)
