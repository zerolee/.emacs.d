;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 一个的保存 point 的实现
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar lzl-point-ring nil
  "这是用来保存 mark 的，理论上无限制")

(defvar lzl-pointer 0
  "当前所在 pointer")

(defvar lzl-ring-mark-pointer nil
  "当前所在 pointer")

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

;;; 比较两个 point 是否相等
(defun struct-point-equal (sp1 sp2)
  (and (= (cdr sp2) (cdr sp1))
       (string-equal (car sp1) (car sp2))))
;;; 从 list 搜索 一个 number，搜索到则返回位置
(defun search-num-from-list (list point count)
  (if list
      (if (struct-point-equal point (car list))
	  count
	(search-num-from-list (cdr list)
			      point
			      (1+ count)))
    -1))

;;; 舍弃第 index 个元素
(defun bury-nth-from-list (list index)
  (if (= index 1)
      (cdr list)
    (cons
     (car list)
     (bury-nth-from-list (cdr list) (1- index)))))


;;; 构造位置信息，(缓冲区对象 point)
(defun lzl-struct-point ()
  (cons (buffer-name) (point)))



(defun lzl-push-mark-to-ring ()
  "将当前位置的 point 存储入 ring， 如果当前位置已经存储过，则从 ring 中删除"
  (interactive)
  (let ((index (search-num-from-list
		lzl-point-ring
		(lzl-struct-point)
		1)))
    (if (> index 0)
	(progn
	  (setq lzl-point-ring (bury-nth-from-list lzl-point-ring index))
	  (if (= lzl-pointer (length lzl-point-ring))
	      (setq lzl-pointer 0))
	  (message "移除当前所在位置的 markpoint"))
      (progn
	(setq lzl-point-ring (cons (lzl-struct-point) lzl-point-ring))
	(message "添加当前位置位 markpoint")))))



(defun lzl-get-mark-from-ring ()
  "得到 lzl-pointer 所指向的 ring 同时，将其往后移动一次"
  (interactive)
  (if (= 0 (length lzl-point-ring))
      (message "point-ring 为空，请先 mark")
    (if (get-buffer (car (nth lzl-pointer lzl-point-ring)))
	(progn
	  (switch-to-buffer (get-buffer
			     (car (nth lzl-pointer lzl-point-ring))))
	  (goto-char (cdr (nth lzl-pointer lzl-point-ring)))
	  (if (=  (length lzl-point-ring) (1+ lzl-pointer))
	      (setq lzl-pointer 0)
	    (setq lzl-pointer (1+ lzl-pointer))))
      (setq lzl-point-ring (bury-nth-from-list lzl-point-ring (1+ lzl-pointer)))
      (if (= (length lzl-point-ring) 1)
	  (setq lzl-pointer 0)
	(lzl-get-mark-from-ring)))))

(provide 'lzl-lib)
