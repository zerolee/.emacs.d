(defhydra hydra-SPC (:color pink
			    :hint nil)
  "
   _c_: counsel       _d_: dired
   _y_: yasnippet     _s_: save-buffer
   _o_: org
  "
  ("c" hydra-counsel/body :exit t)
  ("y" hydra-yasnippet/body :exit t)
  ("s" save-buffer :exit t)
  ("o" hydra-org/body :exit t)
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
(defhydra hydra-org (:color pink
			    :hint nil)
  "
                          Org
   -------------------------------------------------------------
   ^Move^          Title^               ^Time^            ^Operator^
   _p_: 不分级别    _<left>_: 子树降级     _s_: scheduled    _m_: mark
   _n_: 不分级别    _<right>_:子树升级     _t_: state        _q_: tag
   _f_: 同一级别    _<up>_:   子树上移     _d_: deadline
   _b_: 同一级别    _<down>_: 子树下移     _i_: 开始计时
   _U_: 上一级别    _*_:      设为标题     _o_: 停止计时
   _O_: 大纲预览    _/_:      搜索大纲     _._: 时间戳
 "
  ("j" next-line)
  ("k" previous-line)
  ("h" backward-char)
  ("l" forward-char)
  ("u" undo)
  ("I" beginning-of-line :exit t)
  ("a" forward-char :exit t)
  ("A" end-of-line :exit t)
  ("c" hydra-vim/c/body :exit t)
  ("y" hydra-vim/y/body :exit t)
  ("r" hydra-vim/r/body :exit t)
  ("R" hydra-vim/R/body :exit t)
  (";" eval-last-sexp)
  ("p" org-previous-visible-heading)
  ("n" org-next-visible-heading)
  ("f" org-forward-heading-same-level)
  ("b" org-backward-heading-same-level)
  ("U" outline-up-heading)
  ("O" org-goto)
  ("<left>"  org-shiftmetaleft)
  ("<right>" org-shiftmetaright)
  ("<up>"    org-shiftmetaup)
  ("<down>" org-shiftmetadown)
  ("*" org-ctrl-c-star)
  ("/" org-sparse-tree)
  ("s" org-schedule)
  ("d" org-deadline)
  ("t" org-todo)
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("." org-time-stamp)
  ("m" org-ctrl-c-ctrl-c)
  ("q" org-set-tags-command)
  ("<escape>" hydra-esc/body :exit t))


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

(defun lzl-look-forward-char (arg char)
  "查找字符"
  (interactive "*p\ncZap: ")
  (search-forward
   (char-to-string char) nil nil arg))

(defun lzl-vim-get (lzl-move lzl-arg2)
  "删除或者保存 region 中的数据"
  (if (string-match lzl-arg2 "<k")
      (forward-line))
  (if (string-match lzl-arg2 ">jcdy")
      (beginning-of-line))
  (let ((beg (point)))
    (call-interactively lzl-move)
    (funcall lzl-kill-or-save beg (point)))
  (let ((num (prefix-numeric-value current-prefix-arg)))
    (if (< num 0)
	(setq num (- -1 num)))
    (if (string-equal lzl-arg2 "j")
	(setq num (- num 1)))
    (message "%s%d%s" lzl-arg1 num lzl-arg2))
  (if (and (string-equal lzl-arg1 "c")
	   (string-match lzl-arg2 "<>jkc"))
      (open-line 1))
  (setq current-prefix-arg nil)
  (if lzl-esc
      (hydra-esc/body)))

(defhydra hydra-vim/c (:pre (progn (setq lzl-kill-or-save #'kill-region)
				   (setq lzl-esc nil)
				   (setq lzl-arg1 "c"))
			    :color amaranth
			    :hint nil)
  "
  c
  "
  ("<" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (point-min)))
	   (lzl-vim-get #'goto-char "<"))) :exit t)
  (">" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (point-max)))
	   (lzl-vim-get #'goto-char ">"))) :exit t)
  ("i" (lambda ()
	 (interactive)
	 (lzl-vim-get #'beginning-of-line "i")) :exit t)
  ("w" (lambda ()
	 (interactive)
	 (lzl-vim-get #'forward-word "w")) :exit t)
  ("W" (lambda ()
	 (interactive)
	 (lzl-vim-get #'forward-sexp "W")) :exit t)
  (";" (lambda ()
	 (interactive)
	 (lzl-vim-get #'end-of-line ";")) :exit t)
  ("j" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (1+ (prefix-numeric-value current-prefix-arg))))
	   (lzl-vim-get #'forward-line "j"))) :exit t)
  ("k" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (- 0  (1+ (prefix-numeric-value current-prefix-arg)))))
	   (lzl-vim-get #'forward-line "k"))) :exit t)
  ("c" (lambda ()
	 (interactive)
	 (lzl-vim-get #'forward-line "c")) :exit t)
  ("t" (lambda ()
	 (interactive)
	 (lzl-vim-get #'lzl-look-forward-char "t")) :exit t))

(defhydra hydra-vim/d (:pre (progn (setq lzl-kill-or-save #'kill-region)
				   (setq lzl-esc t)
				   (setq lzl-arg1 "d"))
			    :color amaranth
			    :hint nil)
  "
  d
  "
  ("<" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (point-min)))
	   (lzl-vim-get #'goto-char "<"))) :exit t)
  (">" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (point-max)))
	   (lzl-vim-get #'goto-char ">"))) :exit t)
  ("i" (lambda ()
	 (interactive)
	 (lzl-vim-get #'beginning-of-line "i")) :exit t)
  ("w" (lambda ()
	 (interactive)
	 (lzl-vim-get #'forward-word "w")) :exit t)
  ("W" (lambda ()
	 (interactive)
	 (lzl-vim-get #'forward-sexp "W")) :exit t)
  (";" (lambda ()
	 (interactive)
	 (lzl-vim-get #'end-of-line ";")) :exit t)
  ("j" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (1+ (prefix-numeric-value current-prefix-arg))))
	   (lzl-vim-get #'forward-line "j"))) :exit t)
  ("k" (lambda ()
	 (interactive)
	 (let ((current-prefix-arg (- 0  (1+ (prefix-numeric-value current-prefix-arg)))))
	   (lzl-vim-get #'forward-line "k"))) :exit t)
  ("d" (lambda ()
	 (interactive)
	 (lzl-vim-get #'forward-line "d")) :exit t)
  ("t" (lambda ()
	 (interactive)
	 (lzl-vim-get #'lzl-look-forward-char "t")) :exit t))

(defhydra hydra-vim/y (:pre (progn (setq lzl-kill-or-save #'kill-ring-save)
				   (setq lzl-esc t)
				   (setq lzl-arg1 "y"))
			    :color  amaranth
                            :hint nil)
  "
   y
  "
  ("<" (lambda ()
	 (interactive)
	 (save-excursion
	   (let ((current-prefix-arg (point-min)))
	     (lzl-vim-get #'goto-char "<")))) :exit t)
  (">" (lambda ()
	 (interactive)
	 (save-excursion
	   (let ((current-prefix-arg (point-max)))
	     (lzl-vim-get #'goto-char ">")))) :exit t)
  ("i" (lambda ()
	 (interactive)
	 (save-excursion
	   (lzl-vim-get #'beginning-of-line "i"))) :exit t)
  ("w" (lambda ()
	 (interactive)
	 (save-excursion
	   (lzl-vim-get #'forward-word "w"))) :exit t)
  ("W" (lambda ()
	 (interactive)
	 (save-excursion
	   (lzl-vim-get #'forward-sexp "W"))) :exit t)
  (";" (lambda ()
	 (interactive)
	 (save-excursion
	   (lzl-vim-get #'end-of-line ";"))) :exit t)
  ("j" (lambda ()
	 (interactive)
	 (save-excursion
	   (let ((current-prefix-arg (1+ (prefix-numeric-value current-prefix-arg))))
	     (lzl-vim-get #'forward-line "j")))) :exit t)
  ("k" (lambda ()
	 (interactive)
	 (save-excursion
	   (let ((current-prefix-arg (- 0  (1+ (prefix-numeric-value current-prefix-arg)))))
	     (lzl-vim-get #'forward-line "k")))) :exit t)
  ("y" (lambda ()
	 (interactive)
	 (save-excursion
	   (lzl-vim-get #'forward-line "y"))) :exit t)
  ("t"
   (lambda ()
     (interactive)
     (save-excursion
       (lzl-vim-get #'lzl-look-forward-char "t"))) :exit t))

(defun lzl-vim-r ()
  (interactive)
  (undo)
  (self-insert-command 1)
  (delete-char 1))
(defhydra hydra-vim/r (:pre (delete-char 1)
			    :post (hydra-esc/body)
			    :color blue
			    :hint nil)
  ("0" lzl-vim-r)
  ("1" lzl-vim-r)
  ("2" lzl-vim-r)
  ("3" lzl-vim-r)
  ("4" lzl-vim-r)
  ("5" lzl-vim-r)
  ("6" lzl-vim-r)
  ("7" lzl-vim-r)
  ("8" lzl-vim-r)
  ("9" lzl-vim-r)
  ("-" lzl-vim-r)
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
  (let (current-prefix-arg)
    (cond ((= arg 1) (call-interactively #'avy-goto-char))
	  ((= arg 2) (call-interactively #'avy-goto-char-2))
	  ((= arg 3) (call-interactively #'avy-goto-word-1))
	  ((= arg 4) (call-interactively #'avy-goto-word-0))
	  (t (call-interactively #'avy-goto-char-in-line)))))

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
  ("E" paredit-close-round-and-newline)
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
  ("W" forward-whitespace)
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
