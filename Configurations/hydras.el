(defhydra hydra-SPC (:color pink
			    :hint nil)
  "
   _c_: counsel       _d_: dired
   _y_: yasnippet     _o_: org
  "
  ("c" hydra-counsel/body :exit t)
  ("y" hydra-yasnippet/body :exit t)
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
  ("c" (emacs-ckm "c") :exit t)
  ("k" (emacs-ckm "k") :exit t)
  ("y" (emacs-ckm "m") :exit t)
  ("r" hydra-emacs/r/body :exit t)
  ("R" hydra-emacs/R/body :exit t)
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

(defhydra hydra-esc (:color pink
			  :hint nil)
  "
   _<down>_: 取出右边的 s-exp   _<up>_: 去除两边的括号
   _M-s_: (he wo)=>  (he) (wo)  _J_: 将其重新连接起来
   _<left>_: 拿出左边的 s-exp   _<right>_: 拿出右边的 s-exp
  "
  ("SPC" hydra-SPC/body :exit t)
  ("b" ivy-switch-buffer :exit t)
  ("B" lzlvim-B :exit t)
  ("f" counsel-find-file))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs move
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lzl-emacs-get (lzl-move lzl-arg2)
  "删除或者保存 region 中的数据"
  (setq emacs-ckm-point (point))
  (if (string-match lzl-arg2 "<p")
      (end-of-line))
  (if (string-match lzl-arg2 ">nckm")
      (beginning-of-line))
  (funcall lzl-kill-or-save (point)
	   (progn
	     (call-interactively lzl-move)
	     (point)))
  (let ((num (prefix-numeric-value current-prefix-arg)))
    (if (<= num 0)
	(setq num (- 1 num)))
    (if (string-equal lzl-arg2 "n")
	(setq num (- num 1)))
    (message "%s%d%s" lzl-arg1 num lzl-arg2))

  ;; k
  (if (and (string-match lzl-arg1 "k")
	   (string-match lzl-arg2 "<>npk"))
      (let ((pp (point)))
	(if (and  (search-forward "\n" nil  t 1)
		  (= (1+ pp) (point)))
	    (delete-char -1))
	(goto-char pp)))
  ;; 如果复制的话，回复其位置
  (if (string-equal lzl-arg1 "m")
      (goto-char emacs-ckm-point))
  (setq current-prefix-arg nil)
  (if lzl-esc
      (hydra-move/body)))

(defun emacs-ckm (which-ckm)
  (setq lzl-arg1 which-ckm)
  (if (string-equal which-ckm "m")
      (setq lzl-kill-or-save #'kill-ring-save)
    (setq lzl-kill-or-save #'kill-region))
  (if (string-equal which-ckm "c")
      (setq lzl-esc nil)
    (setq lzl-esc t))
  (hydra-emacs/ckm/body))

(defhydra hydra-emacs/ckm (:color blue
				:hint nil)
  ("<" (let ((current-prefix-arg (point-min)))
	 (lzl-emacs-get #'goto-char "<")))
  (">" (let ((current-prefix-arg (point-max)))
	 (lzl-emacs-get #'goto-char ">")))
  ("i" (lzl-emacs-get #'beginning-of-line "i"))
  ("w" (lzl-emacs-get #'forward-word "w"))
  ("s" (lzl-emacs-get #'forward-sexp "s"))
  (";" (lzl-emacs-get #'end-of-line ";"))
  ("n" (let ((current-prefix-arg (1+ (prefix-numeric-value current-prefix-arg))))
	 (lzl-emacs-get #'end-of-line "n")))
  ("p" (let ((current-prefix-arg (- 1  (prefix-numeric-value current-prefix-arg))))
	 (lzl-emacs-get #'beginning-of-line "p")))
  ("c" (lzl-emacs-get #'end-of-line "c"))
  ("k" (lzl-emacs-get #'end-of-line "k"))
  ("m" (lzl-emacs-get #'end-of-line "m"))
  ("t" (lzl-emacs-get #'lzl-look-forward-char "t")))


(defhydra hydra-emacs/r (:body-pre (delete-char 1)
				   :post hydra-move/body
				   :color blue
				   :hint nil)
  ("0" self-insert-command :exit t)
  ("1" self-insert-command :exit t)
  ("2" self-insert-command :exit t)
  ("3" self-insert-command :exit t)
  ("4" self-insert-command :exit t)
  ("5" self-insert-command :exit t)
  ("6" self-insert-command :exit t)
  ("7" self-insert-command :exit t)
  ("8" self-insert-command :exit t)
  ("9" self-insert-command :exit t)
  ("-" self-insert-command :exit t)
  ("<f2>" hydra-move/body :exit t))

(defhydra hydra-emacs/spc (:body-pre (progn
				   (call-interactively #'set-mark-command)
				   (setq-default cursor-type 'bar))
				 :post (setq-default cursor-type t)
				 :color pink
				 :hint nil)
  "
   ---visual---
  "
  ("n" next-line)
  ("p" previous-line)
  ("b" backward-char)
  ("f" forward-char)
  ("a" beginning-of-line)
  ("e" end-of-line)
  ("m" (progn
	 (call-interactively #'kill-ring-save)
	 (hydra-move/body)) :exit t)
  ("M-w" (progn
	 (call-interactively #'kill-ring-save)
	 (hydra-move/body)) :exit t)
  ("w" (progn
	 (call-interactively #'kill-region)
	 (hydra-move/body)) :exit t)
  ("k" (progn
	 (call-interactively #'kill-region)
	 (hydra-move/body)) :exit t)
  ("c" (progn
	 (call-interactively #'kill-region)) :exit t)
  ("t" (progn
	 (call-interactively #'string-rectangle)
	 (hydra-move/body)) :exit t))

(defhydra hydra-emacs/V (:body-pre (progn
				   (rectangle-mark-mode)
				   (setq-default cursor-type 'bar))
				 :post (setq-default cursor-type t)
				 :color pink
				 :hint nil)

  "
   ---rectangle---
  "
  ("n" next-line)
  ("p" previous-line)
  ("b" backward-char)
  ("f" forward-char)
  ("a" beginning-of-line)
  ("e" end-of-line)
  ("m" (progn
	 (call-interactively #'copy-region-as-kill)
	 (hydra-move/body)) :exit t)
  ("M-w" (progn
	 (call-interactively #'copy-region-as-kill)
	 (hydra-move/body)) :exit t)
  ("w" (progn
	 (call-interactively #'kill-region)
	 (hydra-move/body)) :exit t)
  ("k" (progn
	 (call-interactively #'kill-rectangle)
	 (hydra-move/body)) :exit t)
  ("c" (progn
	 (call-interactively #'kill-rectangle)) :exit t)
  ("t" (progn
	 (call-interactively #'string-rectangle)
	 (hydra-move/body)) :exit t))

(defhydra hydra-emacs/R (:body-pre (overwrite-mode)
		       :color pink
			      :hint nil)
  "
   --REPLACE--
  "
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
  ("<f2>"   (progn
		  (overwrite-mode -1)
		  (hydra-move/body)) :exit t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-move (:color pink
			     :hint nil)
  "move"
  ("{" shrink-window-horizontally)
  ("}" enlarge-window-horizontally)
  ("^" enlarge-window)
  ("(" paredit-backward-slurp-sexp)
  (")" paredit-forward-slurp-sexp)
  ("<" paredit-backward-barf-sexp)
  (">" paredit-forward-barf-sexp)
  ("M-s" paredit-split-sexp)
  ("J" paredit-join-sexps)
  ("<up>" paredit-splice-sexp)
  ("<down>" paredit-raise-sexp)
  ("<left>"  paredit-splice-sexp-killing-forward)
  ("<right>" paredit-splice-sexp-killing-backward)
  ("a" beginning-of-line)
  ("A" beginning-of-line :exit t)
  ("C-a" beginning-of-line :exit t)
  ("b" backward-char)
  ("B" lzlvim-B :exit t)
  ("C-b" backward-char :exit t)
  ("c" (emacs-ckm "c") :exit t)
  ("C" kill-line :exit t)
  ("d" delete-char)
  ("D" delete-char :exit t)
  ("C-d" delete-char :exit t)
  ("e" move-end-of-line)
  ("E" move-end-of-line :exit t)
  ("C-e" move-end-of-line :exit t)
  ("f" forward-char)
  ("F" forward-char :exit t)
  ("C-f" forward-char :exit t)
  ("g" avy-goto-line)
  ("G" goto-line)
  ("h" delete-indentation)
  ("i" nil)
  ("I" nil :exit t)
  ("j" paredit-close-round-and-newline :exit t)
  ("k" (emacs-ckm "k") :exit t)
  ("M-k" (progn
	   (kill-whole-line)
	   (open-line 1)) :exit t)
  ("l" recenter-top-bottom)
  ("m" (emacs-ckm "m") :exit t)
  ("n" next-line)
  ("N" (progn
	 (save-excursion
	   (end-of-line)
	   (open-line 1))))
  ("M-n" (progn
	   (end-of-line)
	   (newline)
	   (yank)))
  ("o" (progn
	 (end-of-line)
	 (newline-and-indent)) :exit t)
  ("O" (progn
	 (beginning-of-line)
	 (newline)
	 (forward-line -1)) :exit t)
  ("p" previous-line)
  ("M-p" (progn
	   (beginning-of-line)
	   (open-line 1)
	   (yank)))
  ("P" (progn
	 (save-excursion
	   (beginning-of-line)
	   (open-line 1))))
  ("q" kill-buffer)
  ("r" hydra-emacs/r/body :exit t)
  ("R" hydra-emacs/R/body :exit t)
  ("s" lzl-look-forward-char)
  ("t" avy-goto-char-in-line)
  ("u" undo)
  ("v" scroll-up-command)
  ("V" hydra-emacs/V/body :exit t)
  ("w" avy-goto-word-1)
  ("x" lzl-look-forward-char :exit t)
  ("y" yank)
  ("z" save-buffer)
  ("/" isearch-forward-regexp :exit t)
  ("SPC" hydra-emacs/spc/body :exit t)
  ("[" backward-sexp)
  ("]" forward-sexp)
  (";" eval-last-sexp)
  ("." lzl-push-mark-to-ring)
  ("," lzl-get-mark-from-ring))

(defun lzl-move-n ()
	(interactive)
	(next-line)
	(hydra-move/body))

(defun lzl-move-p ()
	(interactive)
	(previous-line)
	(hydra-move/body))
(define-key text-mode-map (kbd "C-p") #'lzl-move-p)
(define-key prog-mode-map (kbd "C-p") #'lzl-move-p)
(define-key text-mode-map (kbd "C-n") #'lzl-move-n)
(define-key prog-mode-map (kbd "C-n") #'lzl-move-n)
(define-key prog-mode-map (kbd "<f2>") #'hydra-move/body)
(define-key text-mode-map (kbd "<f2>") #'hydra-move/body)
(global-set-key (kbd "<f1>") #'hydra-counsel/body)
