(require 'lzl-lib)
(defhydra hydra-f1 (:color teal
                           :hint nil)
  "
   _l_: locate  _p_: ivy-push-view    _o_: org         _t_: treemacs
   _a_: ag      _P_: ivy-pop-view     _y_: yasnippet   _h_: hs
   _z_: fzf     _r_: rg               _c_: flycheck
   _g_: git     _i_: imenu            _F_: recentf
  "
  ("b" ivy-switch-buffer)
  ("B" lzlvim-B)
  ("f" counsel-find-file)
  ("F" counsel-recentf)
  ("l" counsel-locate)
  ("a" counsel-ag)
  ("z" counsel-fzf)
  ("i" counsel-imenu)
  ("g" counsel-git)
  ("o" hydra-org/body)
  ("p" ivy-push-view)
  ("P" ivy-pop-view)
  ("y" company-yasnippet)
  ("t" treemacs)
  ("H" (hs-minor-mode -1))
  ("h" (progn
         (hs-minor-mode)
         (hs-toggle-hiding)
         (hydra-esc/body)))
  ("r" counsel-rg)
  ("c" flycheck-list-errors)
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
;; hydra-esc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lzlvim-B ()
  "打开并跳转到 ListBuffer"
  (interactive)
  (progn
    (ibuffer-list-buffers)
    (while (not (string-equal "*Ibuffer*" (buffer-name)))
      (other-window 1))))

(defun lzl-look-forward-char (arg char)
  "查找字符"
  (interactive "*p\ncZap: ")
  (setq search-forward-char char)
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
      (hydra-esc/body)))

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
  ("a\"" (progn
           (lzl-look-forward-char -1 ?\")
           (lzl-emacs-get #'(lambda () (interactive)
                              (lzl-look-forward-char 2 ?\")) "a\"")))
  ("a'" (progn
          (lzl-look-forward-char -1 ?')
          (lzl-emacs-get #'(lambda () (interactive)
                             (lzl-look-forward-char 2 ?\')) "a'")))
  ("a\(" (progn
           (lzl-look-forward-char -1 ?\()
           (lzl-emacs-get #'(lambda () (interactive)
                              (lzl-look-forward-char 1 ?\))) "a\)")))
  ("a\[" (progn
           (lzl-look-forward-char -1 ?\[)
           (lzl-emacs-get #'(lambda () (interactive)
                              (lzl-look-forward-char 1 ?\])) "a\]")))
  ("a<" (progn
          (lzl-look-forward-char -1 ?<)
          (lzl-emacs-get #'(lambda () (interactive)
                             (lzl-look-forward-char 1 ?>)) "a>")))
  ("a{" (progn
          (lzl-look-forward-char -1 ?{)
          (lzl-emacs-get #'(lambda () (interactive)
                             (lzl-look-forward-char 1 ?\})) "a\}")))
  ("at" (progn
          (web-mode-element-beginning)
          (lzl-emacs-get #'web-mode-element-end "at")))
  ("e\"" (progn
           (lzl-look-forward-char -1 ?\")
           (forward-char 1)
           (lzl-emacs-get #'(lambda () (interactive)
                              (lzl-look-forward-char 1 ?\")
                              (backward-char 1)) "e\"")))
  ("e'" (progn
          (lzl-look-forward-char -1 ?')
          (forward-char 1)
          (lzl-emacs-get #'(lambda () (interactive)
                             (lzl-look-forward-char 1 ?\')
                             (backward-char 1)) "e'")))
  ("e\(" (progn
           (lzl-look-forward-char -1 ?\()
           (forward-char 1)
           (lzl-emacs-get #'(lambda () (interactive)
                              (lzl-look-forward-char 1 ?\))
                              (backward-char 1)) "e\)")))
  ("e\[" (progn
           (lzl-look-forward-char -1 ?\[)
           (forward-char 1)
           (lzl-emacs-get #'(lambda () (interactive)
                              (lzl-look-forward-char 1 ?\])
                              (backward-char 1)) "e\]")))
  ("e<" (progn
          (lzl-look-forward-char -1 ?<)
          (forward-char 1)
          (lzl-emacs-get #'(lambda () (interactive)
                             (lzl-look-forward-char 1 ?>)
                             (backward-char 1)) "e>")))
  ("e{" (progn
          (lzl-look-forward-char -1 ?{)
          (forward-char 1)
          (lzl-emacs-get #'(lambda () (interactive)
                             (lzl-look-forward-char 1 ?\})
                             (backward-char 1)) "e\}")))
  ("et" (progn
          (web-mode-element-beginning)
          (web-mode-tag-end)
          (lzl-emacs-get #'(lambda () (interactive)
                             (web-mode-element-end)
			     (backward-char 1)
			     (web-mode-tag-beginning)) "at")))
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


(defhydra hydra-emacs/r (:body-pre (delete-char 1)
                                   :post hydra-esc/body
                                   :color blue
                                   :hint nil)
  ("0" (progn
         (insert-char ?0)
         (hydra-esc/body)) :exit t)
  ("1" (progn
         (insert-char ?1)
         (hydra-esc/body)) :exit t)
  ("2" (progn
         (insert-char ?2)
         (hydra-esc/body)) :exit t)
  ("3" (progn
         (insert-char ?3)
         (hydra-esc/body)) :exit t)
  ("4" (progn
         (insert-char ?4)
         (hydra-esc/body)) :exit t)
  ("5" (progn
         (insert-char ?5)
         (hydra-esc/body)) :exit t)
  ("6" (progn
         (insert-char ?6)
         (hydra-esc/body)) :exit t)
  ("7" (progn
         (insert-char ?7)
         (hydra-esc/body)) :exit t)
  ("8" (progn
         (insert-char ?8)
         (hydra-esc/body)) :exit t)
  ("9" (progn
         (insert-char ?9)
         (hydra-esc/body)) :exit t)
  ("-" (progn
         (insert-char ?-)
         (hydra-esc/body)) :exit t)
  ("<escape>" hydra-esc/body :exit t))

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
  ("M-w" (progn
           (call-interactively #'kill-ring-save)
           (hydra-esc/body)) :exit t)
  ("w" (progn
         (call-interactively #'kill-region)
         (hydra-esc/body)) :exit t)
  ("c" (progn
         (call-interactively #'kill-region)) :exit t)
  ("t" (progn
         (call-interactively #'string-rectangle)
         (hydra-esc/body)) :exit t))

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
  ("M-w" (progn
           (call-interactively #'copy-region-as-kill)
           (hydra-esc/body)) :exit t)
  ("w" (progn
         (call-interactively #'kill-region)
         (hydra-esc/body)) :exit t)
  ("c" (progn
         (call-interactively #'kill-rectangle)) :exit t)
  ("t" (progn
         (call-interactively #'string-rectangle)
         (hydra-esc/body)) :exit t))

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
  ("<escape>"   (progn
                  (overwrite-mode -1)
                  (hydra-esc/body)) :exit t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-esc (:color pink
                            :hint nil)
  "
   _<f3>_:gdb  _<f4>_:until  _<f5>_:go  _<f6>_:stop  _<f7>_:step  _<f8>_:next  _<f9>_:cont  _<f10>_:finish
  "
  ("{" shrink-window-horizontally)
  ("}" enlarge-window-horizontally)
  ("^" enlarge-window)
  ("(" paredit-backward-slurp-sexp)
  (")" paredit-forward-slurp-sexp)
  ("<" paredit-backward-barf-sexp)
  (">" paredit-forward-barf-sexp)
  ("=" er/expand-region)
  ("M-s" paredit-split-sexp)
  ("J" paredit-join-sexps)
  ("<up>" paredit-splice-sexp)
  ("<down>" paredit-raise-sexp)
  ("<left>"  paredit-splice-sexp-killing-forward)
  ("<right>" paredit-splice-sexp-killing-backward)
  ("a" beginning-of-line)
  ("C-a" beginning-of-line :exit t)
  ("b" backward-char)
  ("B" lzlvim-B :exit t)
  ("C-b" backward-char :exit t)
  ("c" (emacs-ckm "c") :exit t)
  ("d" delete-char)
  ("C-d" delete-char :exit t)
  ("e" move-end-of-line)
  ("C-e" move-end-of-line :exit t)
  ("f" forward-char)
  ("F" lsp-format-buffer)
  ("C-f" forward-char :exit t)
  ("g" avy-goto-line)
  ("G" goto-line)
  ("h" paredit-backward)
  ("H" delete-indentation)
  ("i" nil)
  ("I" beginning-of-line-text)
  ("j" forward-to-indentation)
  ("C-j" newline-and-indent :exit t)
  ("k" (emacs-ckm "k") :exit t)
  ("l" paredit-forward)
  ("L" (delete-indentation 1))
  ("m" (emacs-ckm "m") :exit t)
  ("M" (save-excursion
         (call-interactively #'mark-whole-buffer)
         (mytab)
         (call-interactively #'untabify)))
  ("n" next-line)
  ("C-n" next-line :exit t)
  ("N" (progn
         (save-excursion
           (end-of-line)
           (open-line 1))))
  ("M-n" (progn
           (end-of-line)
           (newline-and-indent)
           (yank)))
  ("o" (progn
         (end-of-line)
         (newline-and-indent)) :exit t)
  ("O" (progn
         (beginning-of-line)
         (open-line 1)
         (mytab)) :exit t)
  ("p" previous-line)
  ("C-p" previous-line :exit t)
  ("M-p" (progn
           (beginning-of-line)
           (open-line 1)
           (mytab)
           (yank)))
  ("P" (progn
         (save-excursion
           (beginning-of-line)
           (open-line 1))))
  ("q" kill-buffer)
  ("r" hydra-emacs/r/body :exit t)
  ("R" hydra-emacs/R/body :exit t)
  ("s" isearch-forward-regexp :exit t)
  ("t" (progn
         (if (equal 'hydra-esc/lambda-t last-command)
             (lzl-look-forward-char 2 search-forward-char)
           (call-interactively #'lzl-look-forward-char))
         (backward-char)))
  ("u" undo)
  ("U" winner-undo)
  ("v" scroll-up-command)
  ("V" hydra-emacs/V/body :exit t)
  ("w" forward-to-word)
  ("x" (insert-char ?x) :exit t)
  ("y" yank)
  ("z" save-buffer)
  ("Z" save-buffers-kill-terminal)
  ("<C-SPC>" hydra-emacs/spc/body :exit t)
  ("[" paredit-backward-up)
  ("]" paredit-forward-up)
  (";" eval-last-sexp)
  ("." lzl-push-mark-to-ring)
  ("," lzl-get-mark-from-ring)
  ("/" lzl-show-all-mark-in-ring)
  ("M-x" counsel-M-x :exit t)
  ("M-g 1" avy-goto-char)
  ("M-g 2" avy-goto-char-2)
  ("M-g t" avy-goto-char-timer)
  ("M-g f" avy-goto-char-in-line)
  ("M-g l" avy-goto-line)
  ("M-g s" avy-goto-symbol-1)
  ("M-g 0" avy-goto-word-0)
  ("M-g w" avy-goto-word-1)
  ("M-<SPC>" hydra-f1/body :exit t)
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
  ("<escape>" nil))

(unless window-system
  (hydra-esc/body))
