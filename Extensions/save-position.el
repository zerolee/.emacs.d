;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 一个的保存 point 的实现
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ivy)
(defvar sp-position-ring nil
  "这是用来保存 markpoint 的，理论上无限制")

(defvar sp-ring-position-pointer nil
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
(defun sp-goto-buffer (buffer)
  (let ((list (window-list)))
    (while list
      (if (not (string-equal buffer (buffer-name)))
          (other-window 1))
      (setq list (cdr list)))
    (switch-to-buffer (get-buffer buffer))))


;;; 当前位置相关信息 (展示信息：字符串，为了给 ivy 提供显示使用
;;;                位置信息：(buffer-name . name))
(defun sp-context-mark ()
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
  (cons (sp-context-mark)
        (cons (buffer-name) (point-marker))))


(defun index-compute (arg length)
  (let* ((ring-pointer-length (length sp-ring-position-pointer))
         (return-value (+ arg (- length ring-pointer-length))))
    (if (< return-value 0)
        (+ return-value length)
      return-value)))
;;; 将 sp-ring-position-pointer 移动向 sp-position-ring 的下 arg 个元素
(defun rotate-mark-ring-pointer (arg)
  "Rotate the mark point in the mark ring."
  "interactive p"
  (let ((length (length sp-position-ring)))
    (if (zerop length)
        (error "Mark point ring is empty")
      (setq sp-ring-position-pointer
            (nthcdr (% (index-compute arg length)
                       length)
                    sp-position-ring)))))


(defun sp-push-position-to-ring ()
  "将当前位置的 markpoint 存储入 ring， 如果当前位置已经存储过，则从 ring 中删除"
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
  "得到 sp-ring-position-pointer 所指向的 ring 同时，将其往后移动一次"
  (interactive "P")
  (if (zerop (length sp-position-ring))
      (error "position-ring 为空，请先 mark")
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
  (interactive)
  (ivy-read "mark ring: " sp-position-ring
            :action '(lambda (x)
                       (if (zerop (length sp-position-ring))
                           (error "position-ring 为空，请先 mark")
                         (sp-goto-buffer (cadr x))
                         (goto-char (marker-position (cddr x)))))))
(provide 'save-position)
