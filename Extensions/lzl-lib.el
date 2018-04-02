;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 一个的保存 point 的实现
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar lzl-point-ring nil
  "这是用来保存 mark 的，理论上无限制")

(defvar lzl-ring-mark-pointer nil
  "当前所在 pointer")

;;; 比较两个 point 是否相等
(defun struct-point-equal (sp1 sp2)
  (and (= (cdr sp2) (cdr sp1))
       (string-equal (car sp1) (car sp2))))

;;; 舍弃重复的元素
(defun bury-dup-element-from-list (list element)
  (if list
      (if (struct-point-equal (car list) element)
	  (cdr list)
	(cons (car list)
	      (bury-dup-element-from-list (cdr list) element)))
    nil))

;;; 构造位置信息，(缓冲区对象 point)
(defun lzl-struct-point ()
  (cons (buffer-name) (point)))

;;; 将 lzl-ring-mark-pointer 移动向 lzl-point-ring 的下 arg 个元素
(defun rotate-mark-ring-pointer (arg)
  "Rotate the mark point in the mark ring."
  "interactive p"
  (let ((length (length lzl-point-ring)))
    (if (zerop length)
	(error "Mark point ring is empty")

      (setq lzl-ring-mark-pointer
	    (nthcdr (% (+ arg
			  (- length
			     (length
			      lzl-ring-mark-pointer)))
		       length)
		    lzl-point-ring)))))


(defun lzl-push-mark-to-ring ()
  "将当前位置的 point 存储入 ring， 如果当前位置已经存储过，则从 ring 中删除"
  (interactive)
  (let ((pointer
	 (bury-dup-element-from-list lzl-point-ring (lzl-struct-point))))
    (if (= (length pointer) (length lzl-point-ring))
	(progn
	  (setq lzl-point-ring (cons (lzl-struct-point) lzl-point-ring))
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
      (if (get-buffer (caar lzl-ring-mark-pointer))
	  (progn
	    (switch-to-buffer (get-buffer
			       (caar lzl-ring-mark-pointer)))
	    (goto-char (cdar lzl-ring-mark-pointer))
	    (rotate-mark-ring-pointer 1))
	(setq lzl-point-ring
	      (bury-dup-element-from-list lzl-point-ring
					  (car  lzl-ring-mark-pointer)))
	(rotate-mark-ring-pointer 1)
	(lzl-get-mark-from-ring)))))

(provide 'lzl-lib)
