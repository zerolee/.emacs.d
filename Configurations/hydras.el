;;; -*- lexical-binding: t; -*-
(require 'save-position)
(defhydra hydra-f1 (:color teal
                           :hint nil)
  "
   _l_: locate  _p_: ivy-push-view    _o_: org
   _a_: ag      _P_: ivy-pop-view     _y_: yasnippet
   _z_: fzf     _r_: rg               _h_: hs
   _g_: git     _i_: imenu
  "
  ("b" ivy-switch-buffer)
  ("B" goto-ibuffer)
  ("f" counsel-find-file)
  ("l" counsel-locate)
  ("a" counsel-ag)
  ("z" counsel-fzf)
  ("i" counsel-imenu)
  ("g" counsel-git)
  ("o" hydra-org/body)
  ("p" ivy-push-view)
  ("P" ivy-pop-view)
  ("y" company-yasnippet)
  ("H" (hs-minor-mode -1))
  ("h" (progn
         (hs-minor-mode)
         (hs-toggle-hiding)
         (hydra-esc/body)))
  ("r" counsel-rg)
  ("d" dired-jump)
  ("<escape>" hydra-esc/body)
  ("<f1>" nil)
  ("M-<SPC>" nil))


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
;; Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-info (:color red
                             :hint nil)
  "
  _]_ forward  (next logical node)       _l_ast (←)                 _u_p (↑)                           _f_ollow reference
  _[_ backward (prev logical node)       _r_eturn (→)               _m_enu (↓) (C-u for new window)    _d_irectory
  _n_ext (same level only)               _H_istory                  _g_oto (C-u for new window)        _a_propos
  _p_rev (same level only)               _b_eginning of buffer      _e_nd of buffer                    _s_earch (_S_ case sensitive)
  _i_dex item                            _,_ next index item        virtual _I_ndex
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

  ("u"   Info-up)
  ("m"   Info-menu)
  ("g"   Info-goto-node)
  ("b"   beginning-of-buffer)
  ("e"   end-of-buffer)

  ("f"   Info-follow-reference)
  ("i"   Info-index)
  (","   Info-index-next)
  ("I"   Info-virtual-index)

  ("d"   Info-directory)
  ("a"   info-apropos)

  ("?"   Info-summary "Info summary")
  ("h"   Info-help "Info help")
  ("q"   Info-exit "Info exit")
  ("C-g" nil "cancel" :color blue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydra-esc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun goto-ibuffer ()
  "打开并跳转到 Ibuffer"
  (interactive)
  (progn
    (ibuffer-list-buffers)
    (while (not (string-equal "*Ibuffer*" (buffer-name)))
      (other-window 1))))

(defun lzl-look-forward-char (arg char)
  "查找字符"
  (interactive "*p\ncZap: ")
  (search-forward
   (char-to-string char) nil nil arg))

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
  ;; k
  (if (and (string-match lzl-arg1 "k")
           (string-match lzl-arg2 "<>npk"))
      (let ((pp (point)))
        (if (and  (search-forward "\n" nil  t 1)
                  (= (1+ pp) (point)))
            (delete-char -1))
        (goto-char pp)))
  ;; 如果复制的话，恢复其位置
  (if (string-equal lzl-arg1 "m")
      (goto-char emacs-ckm-point))
  (setq current-prefix-arg nil))

(defhydra hydra-emacs/ckm (:color blue
                                  :hint nil)
  "
    %`lzl-arg1
  "
  ("<" (let ((current-prefix-arg (point-min)))
         (lzl-emacs-get #'goto-char "<")))
  (">" (let ((current-prefix-arg (point-max)))
         (lzl-emacs-get #'goto-char ">")))
  ("i" (lzl-emacs-get #'beginning-of-line "i"))
  ("aw" (progn
          (forward-word)
          (backward-word)
          (lzl-emacs-get #'forward-word "aw")))
  ("as" (progn
          (forward-sexp)
          (backward-sexp)
          (lzl-emacs-get #'forward-sexp "as")))
  ("aS" (progn
          (backward-sentence)
          (lzl-emacs-get #'forward-sentence "aS")))
  ("aP" (progn
          (backward-paragraph)
          (lzl-emacs-get #'forward-paragraph "aP")))
  ("a'" (progn
          (lzl-look-forward-char -1 ?')
          (lzl-emacs-get '(lambda () (interactive)
                            (lzl-look-forward-char 2 ?\')) "a'")))
  ("a<" (progn
          (lzl-look-forward-char -1 ?<)
          (lzl-emacs-get '(lambda () (interactive)
                            (let ((flag 1))
                              (forward-char 1)
                              (while (> flag 0)
                                (cond ((char-equal (char-after (point)) ?>) (setq flag (1- flag)))
                                      ((char-equal (char-after (point)) ?<) (setq flag (1+ flag))))
                                (forward-char 1)))) "a>")))
  ("at" (progn
          (web-mode-element-beginning)
          (lzl-emacs-get #'web-mode-element-end "at")))
  ("e'" (progn
          (lzl-look-forward-char -1 ?')
          (forward-char 1)
          (lzl-emacs-get '(lambda () (interactive)
                            (lzl-look-forward-char 1 ?\')
                            (backward-char 1)) "e'")))
  ("e<" (progn
          (lzl-look-forward-char -1 ?<)
          (forward-char 1)
          (lzl-emacs-get '(lambda () (interactive)
                            (let ((flag 1))
                              (while (> flag 0)
                                (cond ((char-equal (char-after (point)) ?>) (setq flag (1- flag)))
                                      ((char-equal (char-after (point)) ?<) (setq flag (1+ flag))))
                                (forward-char 1)))
                            (backward-char 1)) "e>")))

  ("et" (progn
          (web-mode-element-beginning)
          (web-mode-tag-end)
          (lzl-emacs-get '(lambda () (interactive)
                            (web-mode-element-end)
                            (backward-char 1)
                            (web-mode-tag-beginning)) "at")))
  ("el" (progn
          (paredit-backward-up)
          (forward-char 1)
          (lzl-emacs-get '(lambda () (interactive)
                            (paredit-forward-up)
                            (backward-char 1)) "el")))
  ("l" (progn
         (paredit-backward-up)
         (lzl-emacs-get #'forward-sexp "s")))
  ("d" (progn
         (end-of-defun)
         (beginning-of-defun)
         (lzl-emacs-get #'forward-sexp "s")))
  ("S" (lzl-emacs-get #'forward-sentence "S"))
  ("P" (lzl-emacs-get #'forward-paragraph "P"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-gud (:color pink
                            :hint nil)
  "
   _<f3>_:gdb  _<f4>_:until  _<f5>_:go  _<f6>_:stop  _<f7>_:step  _<f8>_:next  _<f9>_:cont  _<f10>_:finish
  "
  ("<f3>" (progn
            (call-interactively #'gdb-many-windows)
            (call-interactively #'tool-bar-mode)))
  ("<f4>" gud-until)
  ("<f5>" gud-go)
  ("<f6>" gud-stop-subjob)
  ("<f7>" gud-step)
  ("<f8>" gud-next)
  ("<f9>" gud-cont)
  ("<f10>" gud-finish)
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
  ("" nil))
