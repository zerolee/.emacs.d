(defhydra hydra-SPC (:color pink
			    :hint nil)
  "
   _c_: counsel       _d_: dired
   _y_: yasnippet     _s_: save-buffer
  "
  ("c" hydra-counsel/body :exit t)
  ("y" hydra-yasnippet/body :exit t)
  ("s" save-buffer :exit t)
  ("d" lzl-dired :exit t)
  ("i" nil "cancel")
  ("<escape>" hydra-esc/body :exit t)
  ("q" kill-buffer "quit" :color blue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-counsel (:color pink
				:hint nil)
  "
                   counsel
   -------------------------------------------------------------
   _l_: locate  _p_: ivy-push-view
   _a_: ag      _P_: ivy-pop-view
   _f_: fzf     _r_: ivy-resume
   _i_: imenu
   _g_: git
  "
  ("l" counsel-locate :exit t)
  ("a" counsel-ag :exit t)
  ("f" counsel-fzf :exit t)
  ("i" counsel-imenu :exit t)
  ("g" counsel-git :exit t)
  ("p" ivy-push-view :exit t)
  ("P" ivy-pop-view :exit t)
  ("r" ivy-resume :exit t)
  ("c" nil "cancel")
  ("q" kill-buffer "quit" :color blue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-yasnippet (:color pink
				  :hint nil)
  "
                      yasnippet
   -------------------------------------------------------------
   _y_: company-yasnippet  
   _i_: yas-insert-snippet _n_: yas-new-snippet _e_: yas-expand
  "
  ("y" company-yasnippet :exit t)
  ("i" yas-insert-snippet :exit t)
  ("n" yas-new-snippet :exit t)
  ("e" yas-expand :exit t)
  ("c" nil "cancel")
  ("q" kill-buffer "quit" :color blue))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydra-esc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lzlvim-B ()
  "打开并跳转到 ListBuffer"
  (interactive)
  (progn
    (list-buffers)
    (while (not (string-equal "*Buffer List*" (buffer-name)))
      (other-window 1))))

(defhydra hydra-vim/c (:color amaranth
			      :hint nil)
  "
   c
  "
  ("<" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (count-lines (point-min) (point))))
	   (goto-char (point-min))
	   (call-interactively #'kill-whole-line))) :exit t)
  (">" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (count-lines (point) (point-max))))
	   (call-interactively #'kill-whole-line))) :exit t)
  ("i" (lambda ()
	 (interactive)
	 (let ((end (point)))
	   (beginning-of-line)
	   (delete-char (- end (point))))) :exit t)
  ("w" (lambda ()
	 (interactive)
	 (call-interactively #'kill-word)
	 (message "c%dw" (prefix-numeric-value current-prefix-arg)))  :exit t)
  ("W" (lambda ()
	 (interactive)
	 (call-interactively #'kill-sexp)
	 (message "c%dW" (prefix-numeric-value current-prefix-arg))) :exit t)
  (";" (lambda ()
	 (interactive)
	 (kill-line)
	 (message "c;")) :exit t)
  ("j" (lambda ()
	 (interactive)
	 (beginning-of-line)
	 (let ((current-prefix-arg
		(1+ (prefix-numeric-value current-prefix-arg))))
	   (call-interactively #'kill-line)
	   (open-line 1)
	   (message "c%dj" (1- current-prefix-arg)))) :exit t)
  ("k" (lambda ()
	 (interactive)
	 (beginning-of-line)
	 (forward-line (- 0 (prefix-numeric-value current-prefix-arg)))
	 (let ((current-prefix-arg
		(1+ (prefix-numeric-value current-prefix-arg))))
	   (call-interactively #'kill-line)
	   (open-line 1)
	   (message "c%dk" (1- current-prefix-arg)))) :exit t)
  ("c" (lambda ()
	 (interactive)
	 (beginning-of-line)
	 (if (= (prefix-numeric-value current-prefix-arg) 1)
	     (kill-line)
	   (progn
	     (call-interactively #'kill-line)
	     (open-line 1)))
	 (message "c%dc" (prefix-numeric-value current-prefix-arg))) :exit t))


(defhydra hydra-vim/d (:color amaranth
			      :hint nil)
  "
  d
  "
  ("<" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (count-lines (point-min) (point))))
	   (goto-char (point-min))
	   (call-interactively #'kill-whole-line))
	 (message "d<")
	 (hydra-esc/body)) :exit t)
  (">" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (count-lines (point) (point-max))))
	   (call-interactively #'kill-whole-line))
	 (message "d>")
	 (hydra-esc/body)) :exit t)
  ("i" (lambda ()
	 (interactive)
	 (let ((end (point)))
	   (beginning-of-line)
	   (delete-char (- end (point))))
	 (message "di")
	 (hydra-esc/body)) :exit t)
  ("w" (lambda ()
	 (interactive)
	 (call-interactively #'kill-word)
	 (message "d%dw" (prefix-numeric-value current-prefix-arg))
	 (setq current-prefix-arg nil)
	 (hydra-esc/body))  :exit t)
  ("W" (lambda ()
	 (interactive)
	 (call-interactively #'kill-sexp)
	 (message "d%dW" (prefix-numeric-value current-prefix-arg))
	 (setq current-prefix-arg nil)
	 (hydra-esc/body)) :exit t)
  (";" (lambda ()
	 (interactive)
	 (kill-line)
	 (message "d;")
	 (hydra-esc/body)) :exit t)
  ("j" (lambda ()
	 (interactive)
	 (beginning-of-line)
	 (let ((current-prefix-arg
		(1+ (prefix-numeric-value current-prefix-arg))))
	   (call-interactively #'kill-line)
	   (message "d%dj" (1- current-prefix-arg)))
	 (setq current-prefix-arg nil)
	 (hydra-esc/body)) :exit t)
  ("k" (lambda ()
	 (interactive)
	 (beginning-of-line)
	 (forward-line (- 0 (prefix-numeric-value current-prefix-arg)))
	 (let ((current-prefix-arg
		(1+ (prefix-numeric-value current-prefix-arg))))
	   (call-interactively #'kill-line)
	   (message "d%dk" (1- current-prefix-arg)))
	 (setq current-prefix-arg nil)
	 (hydra-esc/body)) :exit t)
  ("d" (lambda ()
	 (interactive)
	 (beginning-of-line)
	 (if (= (prefix-numeric-value current-prefix-arg) 1)
	     (progn (kill-line)
		    (delete-char 1))
	   (call-interactively #'kill-line))
	 (message "d%dd" (prefix-numeric-value current-prefix-arg))
	 (setq current-prefix-arg nil)
	 (hydra-esc/body)) :exit t))

(defun lzl-vim-y (arg)
  "复制 arg + 1 行数据"
  (interactive)
  (save-excursion
    (let (beg)
      (beginning-of-line)
      (setq beg (point))
      (push-mark)
      (forward-line arg)
      (end-of-line)
      (kill-ring-save beg (point)))))

(defhydra hydra-vim/y (:color  pink
                               :hint nil)
  "
   y
  "
  ("i" (lambda ()
	 (interactive)
	 (save-excursion
	   (let ((end (point)))
	     (push-mark)
	     (beginning-of-line)
	     (kill-ring-save  (point) end)))
	 (message "yi")
	 (hydra-esc/body)) :exit t)
  (";" (lambda ()
	 (interactive)
	 (save-excursion
	   (let ((beg (point)))
	     (push-mark)
	     (end-of-line)
	     (kill-ring-save beg (point))))
	 (message "y;")
	 (hydra-esc/body)) :exit t)
  ("w" (lambda ()
	 (interactive)
	 (save-excursion
	   (let ((beg (point)))
	     (push-mark)
	     (forward-word (prefix-numeric-value current-prefix-arg))
	     (kill-ring-save beg (point))))
	 (message "y%dw" (prefix-numeric-value current-prefix-arg))
	 (setq current-prefix-arg nil)
	 (hydra-esc/body)) :exit t)
  ("<" (lambda ()
	 (interactive)
	 (save-excursion
	   (let ((count (count-lines (point-min) (point))))
	     (goto-char (point-min))
	     (lzl-vim-y (1- count))))
	 (message "y<")
	 (hydra-esc/body)) :exit t)
  (">" (lambda ()
	 (interactive)
	 (lzl-vim-y (1- (count-lines (point) (point-max))))
	 (message "y>")
	 (hydra-esc/body)) :exit t)
  ("j"
   (lambda ()
     (interactive)
     (lzl-vim-y (prefix-numeric-value current-prefix-arg))
     (message "y%dj" (prefix-numeric-value current-prefix-arg))
     (setq current-prefix-arg nil)
     (hydra-esc/body)) :exit t)
  ("k" (lambda ()
	 (interactive)
	 (save-excursion
	   (forward-line (- 0 (prefix-numeric-value current-prefix-arg)))
	   (lzl-vim-y (prefix-numeric-value current-prefix-arg)))
	 (message "y%dk" (prefix-numeric-value current-prefix-arg))
	 (hydra-esc/body)) :exit t)
  ("y"
   (lambda ()
     (interactive)
     (lzl-vim-y (1- (prefix-numeric-value current-prefix-arg)))
     (message "y%dy" (prefix-numeric-value current-prefix-arg))
     (setq current-prefix-arg nil)
     (hydra-esc/body)) :exit t))


(defhydra hydra-vim/r (:pre (delete-char 1)
			    :post (hydra-esc/body)
			    :color blue
			    :hint nil)
  ("<escape>" hydra-esc/body :exit t))

(defhydra hydra-vim/v (:pre (setq-default cursor-type 'bar)
			    :color pink
			    :hint nil)

  ("j" next-line)
  ("k" previous-line)
  ("h" backward-char)
  ("l" forward-char)
  ("y" (lambda ()
	 (interactive)
	 (call-interactively #'kill-ring-save)
	 (hydra-esc/body)) :exit t)
  ("d" (lambda ()
	 (interactive)
	 (call-interactively #'kill-region)
	 (hydra-esc/body)) :exit t)
  ("c" (lambda ()
	 (interactive)
	 (call-interactively #'kill-region)
	 (setq-default cursor-type t)) :exit t)
  ("t" (lambda ()
	 (interactive)
	 (call-interactively #'string-rectangle)
	 (hydra-esc/body)) :exit t))

(defhydra hydra-vim/V (:pre (setq-default cursor-type 'bar)
			    :color pink
			    :hint nil)

  ("j" next-line)
  ("k" previous-line)
  ("h" backward-char)
  ("l" forward-char)
  ("y" (lambda ()
	 (interactive)
	 (call-interactively #'copy-region-as-kill)
	 (hydra-esc/body)) :exit t)
  ("d" (lambda ()
	 (interactive)
	 (call-interactively #'kill-rectangle)
	 (hydra-esc/body)) :exit t)
  ("c" (lambda ()
	 (interactive)
	 (call-interactively #'kill-rectangle)
	 (setq-default cursor-type t)) :exit t)
  ("t" (lambda ()
	 (interactive)
	 (call-interactively #'string-rectangle)
	 (hydra-esc/body)) :exit t))

(defhydra hydra-vim/R (:color pink
			      :hint nil)
  "
   --REPLACE--
  "
  ("<escape>"
   (lambda ()
     (interactive)
     (overwrite-mode -1)
     (hydra-esc/body)) :exit t))

(defun lzl-avy (arg)
  (interactive "p")
  (cond ((= arg 1) (call-interactively #'avy-goto-char))
	((= arg 2) (call-interactively #'avy-goto-char-2))
	((= arg 3) (call-interactively #'avy-goto-word-1))
	((= arg 4) (call-interactively #'avy-goto-word-0))
	(t (call-interactively #'avy-goto-char-in-line))))

(defhydra hydra-esc (:pre (setq-default cursor-type t)
			  :color pink
			  :hint nil)
  "
   _<down>_: 取出右边的 s-exp   _<up>_: 去除两边的括号
   _S_: (he wo)=>  (he) (wo)  _J_: 将其重新连接起来
   _<left>_: 拿出左边的 s-exp   _<right>_: 拿出右边的 s-exp
  "
  ("(" paredit-backward-slurp-sexp)
  (")" paredit-forward-slurp-sexp)
  ("<" paredit-backward-barf-sexp)
  (">" paredit-forward-barf-sexp)
  ("S" paredit-split-sexp)
  ("J" paredit-join-sexps)
  ("<up>" paredit-splice-sexp)
  ("<down>" paredit-raise-sexp)
  ("<left>"  paredit-splice-sexp-killing-forward)
  ("<right>" paredit-splice-sexp-killing-backward)
  ("SPC" hydra-SPC/body :exit t)
  ("a" forward-char :exit t)
  ("A" move-end-of-line :exit t)
  ("b" ivy-switch-buffer)
  ("B" lzlvim-B :exit t)
  ("c" hydra-vim/c/body :exit t)
  ("C" kill-line :exit t)
  ("d" hydra-vim/d/body :exit t)
  ("D" kill-line)
  ("e" forward-word)
  ("E" eval-last-sexp)
  ("f" counsel-find-file)
  ("F" avy-goto-char-in-line)
  ("g" avy-goto-line)
  ("G" goto-line)
  ("h" backward-char)
  ("H" beginning-of-buffer)
  ("i" nil "cancel")
  ("I" move-beginning-of-line :exit t)
  ("j" next-line)
  ("k" previous-line)
  ("K" (lambda ()
	 (interactive)
	 (beginning-of-line)
	 (kill-line)) :exit t)
  ("l" forward-char)
  ("L" delete-indentation)
  ("m" point-to-register)
  ("M" window-configuration-to-register)
  ("n" (lambda ()
	 (interactive)
	 (save-excursion
	   (end-of-line)
	   (call-interactively #'open-line))))
  ("N" (lambda ()
	 (interactive)
	 (save-excursion
	   (beginning-of-line)
	   (call-interactively #'newline))))
  ("o" (lambda ()
	 (interactive)
	 (end-of-line)
	 (newline-and-indent)) :exit t)
  ("O" (lambda ()
	 (interactive)
	 (beginning-of-line)
	 (newline)
	 (forward-line -1)) :exit t)
  ("p" (lambda ()
	 (interactive)
	 (end-of-line)
	 (newline)
	 (yank)))
  ("P" (lambda ()
	 (interactive)
	 (beginning-of-line)
	 (newline)
	 (forward-line -1)
	 (yank)))
  ("q" backward-word)
  ("Q" kill-buffer "quit" :color blue)
  ("r" hydra-vim/r/body :exit t)
  ("R" (lambda ()
	 (interactive)
	 (overwrite-mode)
	 (hydra-vim/R/body)) :exit t)
  ("s" delete-char :exit t)
  ("t" lzl-avy)
  ("T" transpose-lines)
  ("u" undo)
  ("v" (lambda ()
	 (interactive)
	 (call-interactively #'set-mark-command)
	 (hydra-vim/v/body)) :exit t)
  ("V" (lambda ()
	 (interactive)
	 (rectangle-mark-mode)
	 (hydra-vim/V/body)) :exit t)
  ("w" (lambda ()
	 (interactive)
	 (forward-word)
	 (forward-word)
	 (backward-word)))
  ("W" avy-goto-word-0)
  ("x" delete-char)
  ("X" delete-backward-char)
  ("y" hydra-vim/y/body :exit t)
  ("z" save-buffer)
  ("Z" save-buffers-kill-terminal :exit t)
  (";" (lambda ()
	 (interactive)
	 (end-of-line)
	 (call-interactively #'eval-last-sexp)))
  ("/" isearch-forward-regexp :exit t)
  ("{" shrink-window-horizontally)
  ("}" enlarge-window-horizontally)
  ("[" backward-sexp)
  ("]" forward-sexp)
  ("^" enlarge-window)
  ("M-h" windmove-left)
  ("M-j" windmove-down)
  ("M-k" windmove-up)
  ("M-l" windmove-right)
  ("M-0" delete-window)
  ("M-1" delete-other-windows)
  ("M-2" split-window-below)
  ("M-3" split-window-right)
  ("M-x" counsel-M-x)
  ("`" jump-to-register)
  ("!" quit-window)
  ("$" end-of-line)
  ("." hydra-repeat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 配置 buffer-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-buffer-menu (:color pink
				    :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))
