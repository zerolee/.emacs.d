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
			    :color blue
			    :hint nil)
  "
  c
  "
  ("<" (let ((current-prefix-arg (point-min)))
	 (lzl-vim-get #'goto-char "<")))
  (">" (let ((current-prefix-arg (point-max)))
	 (lzl-vim-get #'goto-char ">")))
  ("i" (lzl-vim-get #'beginning-of-line "i"))
  ("w" (lzl-vim-get #'forward-word "w"))
  ("W" (lzl-vim-get #'forward-sexp "W"))
  (";" (lzl-vim-get #'end-of-line ";"))
  ("j" (let ((current-prefix-arg (1+ (prefix-numeric-value current-prefix-arg))))
	 (lzl-vim-get #'forward-line "j")))
  ("k" (let ((current-prefix-arg (- 0  (1+ (prefix-numeric-value current-prefix-arg)))))
	 (lzl-vim-get #'forward-line "k")))
  ("c" (lzl-vim-get #'forward-line "c"))
  ("t" (lzl-vim-get #'lzl-look-forward-char "t")))

(defhydra hydra-vim/d (:pre (progn (setq lzl-kill-or-save #'kill-region)
				   (setq lzl-esc t)
				   (setq lzl-arg1 "d"))
			    :color blue
			    :hint nil)
  "
  d
  "
  ("<" (let ((current-prefix-arg (point-min)))
	 (lzl-vim-get #'goto-char "<")))
  (">" (let ((current-prefix-arg (point-max)))
	 (lzl-vim-get #'goto-char ">")))
  ("i" (lzl-vim-get #'beginning-of-line "i"))
  ("w" (lzl-vim-get #'forward-word "w"))
  ("W" (lzl-vim-get #'forward-sexp "W"))
  (";" (lzl-vim-get #'end-of-line ";"))
  ("j" (let ((current-prefix-arg (1+ (prefix-numeric-value current-prefix-arg))))
	 (lzl-vim-get #'forward-line "j")))
  ("k" (let ((current-prefix-arg (- 0  (1+ (prefix-numeric-value current-prefix-arg)))))
	 (lzl-vim-get #'forward-line "k")))
  ("d" (lzl-vim-get #'forward-line "d"))
  ("t" (lzl-vim-get #'lzl-look-forward-char "t")))

(defhydra hydra-vim/y (:pre (progn (setq lzl-kill-or-save #'kill-ring-save)
				   (setq lzl-esc t)
				   (setq lzl-arg1 "y"))
			    :color  blue
                            :hint nil)
  "
   y
  "
  ("<" (save-excursion
	 (let ((current-prefix-arg (point-min)))
	   (lzl-vim-get #'goto-char "<"))))
  (">" (save-excursion
	 (let ((current-prefix-arg (point-max)))
	   (lzl-vim-get #'goto-char ">"))))
  ("i" (save-excursion
	 (lzl-vim-get #'beginning-of-line "i")))
  ("w" (save-excursion
	 (lzl-vim-get #'forward-word "w")))
  ("W" (save-excursion
	 (lzl-vim-get #'forward-sexp "W")))
  (";" (save-excursion
	 (lzl-vim-get #'end-of-line ";")))
  ("j" (save-excursion
	 (let ((current-prefix-arg (1+ (prefix-numeric-value current-prefix-arg))))
	   (lzl-vim-get #'forward-line "j"))))
  ("k" (save-excursion
	 (let ((current-prefix-arg (- 0  (1+ (prefix-numeric-value current-prefix-arg)))))
	   (lzl-vim-get #'forward-line "k"))))
  ("y" (save-excursion
	 (lzl-vim-get #'forward-line "y")))
  ("t" (save-excursion
	 (lzl-vim-get #'lzl-look-forward-char "t"))))

(defhydra hydra-vim/r (:body-pre (delete-char 1)
			    :post (hydra-esc/body)
			    :color blue
			    :hint nil)
  ("0" self-insert-command)
  ("1" self-insert-command)
  ("2" self-insert-command)
  ("3" self-insert-command)
  ("4" self-insert-command)
  ("5" self-insert-command)
  ("6" self-insert-command)
  ("7" self-insert-command)
  ("8" self-insert-command)
  ("9" self-insert-command)
  ("-" self-insert-command)
  ("<escape>" hydra-esc/body :exit t))

(defhydra hydra-vim/v (:body-pre (progn
				   (call-interactively #'set-mark-command)
				   (setq-default cursor-type 'bar))
				 :post (setq-default cursor-type t)
				 :color pink
				 :hint nil)

  ("j" next-line)
  ("k" previous-line)
  ("h" backward-char)
  ("l" forward-char)
  ("y" (progn
	 (call-interactively #'kill-ring-save)
	 (hydra-esc/body)) :exit t)
  ("d" (progn
	 (call-interactively #'kill-region)
	 (hydra-esc/body)) :exit t)
  ("c" (progn
	 (call-interactively #'kill-region)
	 (setq-default cursor-type t)) :exit t)
  ("t" (progn
	 (call-interactively #'string-rectangle)
	 (hydra-esc/body)) :exit t))

(defhydra hydra-vim/V (:body-pre (progn
				   (rectangle-mark-mode)
				   (setq-default cursor-type 'bar))
				 :post (setq-default cursor-type t)
				 :color pink
				 :hint nil)

  ("j" next-line)
  ("k" previous-line)
  ("h" backward-char)
  ("l" forward-char)
  ("y" (progn
	 (call-interactively #'copy-region-as-kill)
	 (hydra-esc/body)) :exit t)
  ("d" (progn
	 (call-interactively #'kill-rectangle)
	 (hydra-esc/body)) :exit t)
  ("c" (progn
	 (call-interactively #'kill-rectangle)
	 (setq-default cursor-type t)) :exit t)
  ("t" (progn
	 (call-interactively #'string-rectangle)
	 (hydra-esc/body)) :exit t))

(defhydra hydra-vim/R (:body-pre (overwrite-mode)
		       :color pink
			      :hint nil)
  "
   --REPLACE--
  "
  ("<escape>"   (progn
		  (overwrite-mode -1)
		  (hydra-esc/body)) :exit t))


(defun lzl-avy (arg)
  (interactive "p")
  (let (current-prefix-arg)
    (cond ((= arg 3) (call-interactively #'avy-goto-char))
	  ((= arg 2) (call-interactively #'avy-goto-char-2))
	  ((= arg 1) (call-interactively #'avy-goto-word-1))
	  ((= arg 4) (call-interactively #'avy-goto-word-0))
	  (t (call-interactively #'avy-goto-char-in-line)))))

(defhydra hydra-esc (:color pink
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
  ("b" ivy-switch-buffer :exit t)
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
  ("K" (progn
	 (beginning-of-line)
	 (kill-line)) :exit t)
  ("l" forward-char)
  ("L" delete-indentation)
  ("m" point-to-register)
  ("M" window-configuration-to-register)
  ("n" (progn
	 (save-excursion
	   (end-of-line)
	   (call-interactively #'open-line))))
  ("N" (progn
	 (save-excursion
	   (beginning-of-line)
	   (call-interactively #'newline))))
  ("o" (progn
	 (end-of-line)
	 (newline-and-indent)) :exit t)
  ("O" (progn
	 (beginning-of-line)
	 (newline)
	 (forward-line -1)) :exit t)
  ("p" (progn
	 (end-of-line)
	 (newline)
	 (yank)))
  ("P" (progn
	 (beginning-of-line)
	 (newline)
	 (forward-line -1)
	 (yank)))
  ("q" backward-word)
  ("Q" kill-buffer "quit" :color blue)
  ("r" hydra-vim/r/body :exit t)
  ("R" hydra-vim/R/body :exit t)
  ("s" delete-char :exit t)
  ("t" lzl-avy)
  ("T" transpose-lines)
  ("u" undo)
  ("v" hydra-vim/v/body :exit t)
  ("V" hydra-vim/V/body :exit t)
  ("w" (progn
	 (forward-word)
	 (forward-word)
	 (backward-word)))
  ("W" forward-whitespace)
  ("x" delete-char)
  ("X" delete-backward-char)
  ("y" hydra-vim/y/body :exit t)
  ("z" save-buffer)
  ("Z" save-buffers-kill-terminal :exit t)
  (";" (progn
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-info (:color red
                      :hint nil)
      "
Info-mode:

  ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
  ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
  ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos

  _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
      ("]"   Info-forward-node)
      ("["   Info-backward-node)
      ("n"   Info-next)
      ("p"   Info-prev)
      ("s"   Info-search)
      ("S"   Info-search-case-sensitively)

      ("l"   Info-history-back)
      ("r"   Info-history-forward)
      ("H"   Info-history)
      ("t"   Info-top-node)
      ("<"   Info-top-node)
      (">"   Info-final-node)

      ("u"   Info-up)
      ("^"   Info-up)
      ("m"   Info-menu)
      ("g"   Info-goto-node)
      ("b"   beginning-of-buffer)
      ("e"   end-of-buffer)

      ("f"   Info-follow-reference)
      ("i"   Info-index)
      (","   Info-index-next)
      ("I"   Info-virtual-index)

      ("T"   Info-toc)
      ("d"   Info-directory)
      ("c"   Info-copy-current-node-name)
      ("C"   clone-buffer)
      ("a"   info-apropos)

      ("1"   Info-nth-menu-item)
      ("2"   Info-nth-menu-item)
      ("3"   Info-nth-menu-item)
      ("4"   Info-nth-menu-item)
      ("5"   Info-nth-menu-item)
      ("6"   Info-nth-menu-item)
      ("7"   Info-nth-menu-item)
      ("8"   Info-nth-menu-item)
      ("9"   Info-nth-menu-item)

      ("?"   Info-summary "Info summary")
      ("h"   Info-help "Info help")
      ("q"   Info-exit "Info exit")
      ("C-g" nil "cancel" :color blue))

(defhydra hydra-move
    (:body-pre (next-line))
    "move"
    ("n" next-line)
    ("p" previous-line)
    ("f" forward-char)
    ("b" backward-char)
    ("a" beginning-of-line)
    ("A" move-end-of-line :exit t)
    ("i" nil)
    ("I" move-beginning-of-line :exit t)
    ("e" move-end-of-line)
    ("v" scroll-up-command)
    ;; Converting M-v to V here by analogy.
    ("V" scroll-down-command)
    ("o" (progn
	   (end-of-line)
	   (newline-and-indent)) :exit t)
    ("O" (progn
	   (beginning-of-line)
	   (newline)
	   (forward-line -1)) :exit t)
    ("k" kill-line)
    ("K" kill-whole-line)
    ("d" delete-char)
    ("D" kill-word)
    ("y" yank)
    ("Y" (progn
	 (end-of-line)
	 (newline)
	 (yank)))
    ("P" (progn
	 (beginning-of-line)
	 (newline)
	 (forward-line -1)
	 (yank)))
    ("u" undo)
    ("l" recenter-top-bottom)
    ("s" isearch-forward-regexp)
    ("w" kill-sexp)
    ("c" (save-excursion
	   (beginning-of-line)
	   (let ((beg (point)))
	     (end-of-line)
	     (kill-ring-save beg (point)))))
    ("[" backward-sexp)
    ("]" forward-sexp)
    ("z" save-buffer))
(define-key text-mode-map (kbd "C-n") #'hydra-move/body)
(define-key prog-mode-map (kbd "C-n") #'hydra-move/body)


(defhydra hydra-move-p
    (:body-pre (previous-line))
    "move"
    ("n" next-line)
    ("p" previous-line)
    ("f" forward-char)
    ("b" backward-char)
    ("a" beginning-of-line)
    ("A" move-end-of-line :exit t)
    ("i" nil)
    ("I" move-beginning-of-line :exit t)
    ("e" move-end-of-line)
    ("v" scroll-up-command)
    ;; Converting M-v to V here by analogy.
    ("V" scroll-down-command)
    ("o" (progn
	   (end-of-line)
	   (newline-and-indent)) :exit t)
    ("O" (progn
	   (beginning-of-line)
	   (newline)
	   (forward-line -1)) :exit t)
    ("k" kill-line)
    ("K" kill-whole-line)
    ("d" delete-char)
    ("D" kill-word)
    ("y" yank)
    ("Y" (progn
	 (end-of-line)
	 (newline)
	 (yank)))
    ("P" (progn
	 (beginning-of-line)
	 (newline)
	 (forward-line -1)
	 (yank)))
    ("u" undo)
    ("l" recenter-top-bottom)
    ("s" isearch-forward-regexp)
    ("w" kill-sexp)
    ("c" (save-excursion
	   (beginning-of-line)
	   (let ((beg (point)))
	     (end-of-line)
	     (kill-ring-save beg (point)))))
    ("[" backward-sexp)
    ("]" forward-sexp)
    ("z" save-buffer))
(define-key text-mode-map (kbd "C-p") #'hydra-move-p/body)
(define-key prog-mode-map (kbd "C-p") #'hydra-move-p/body)
