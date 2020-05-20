;;; -*- lexical-binding: t; -*-
(require 'zerolee-lib)
(defhydra hydra-f1 (:color teal
                           :hint nil)
  "
   _l_: locate  _p_: ivy-push-view    _y_: yasnippet
   _a_: ag      _P_: ivy-pop-view     _h_: hs
   _z_: fzf     _r_: rg               _m_: emms-default
   _g_: git     _i_: imenu            _M_: emms-favourite
  "
  ("b" ivy-switch-buffer)
  ("B" goto-ibuffer)
  ("f" counsel-find-file)
  ("l" counsel-locate)
  ("a" counsel-ag)
  ("z" counsel-fzf)
  ("i" counsel-imenu)
  ("g" counsel-git)
  ("p" ivy-push-view)
  ("P" ivy-pop-view)
  ("y" company-yasnippet)
  ("H" (hs-minor-mode -1))
  ("h" (progn
         (hs-minor-mode)
         (hs-toggle-hiding)))
  ("r" counsel-rg)
  ("d" (dired default-directory))
  ("m" zerolee-emms-default)
  ("M" zerolee-emms-favourite)
  ("<f1>" nil)
  ("M-<SPC>" nil))

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
  ("q"   quit-window "Info exit")
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

(defhydra hydra-emacs/ckm (:color blue
                                  :hint nil)
  ("<" (let ((current-prefix-arg (point-min)))
         (ove-emacs-get #'goto-char "<")))
  (">" (let ((current-prefix-arg (point-max)))
         (ove-emacs-get #'goto-char ">")))
  ("i" (ove-emacs-get #'beginning-of-line "i"))
  ("," (ove-emacs-get #'ove-function-arg-end ","))
  ("e," (progn
          (ove-function-arg-begin)
          (ove-emacs-get #'ove-function-arg-end ",")))
  ("a," (progn
          (ove-function-arg-begin)
          (ove-emacs-get #'ove-function-arg-end ",")
          (when (char-equal (char-after) ?\,)
            (delete-char 1))
          (when (char-equal (char-after) ?\ )
            (delete-char 1))))
  ("aw" (progn
          (forward-word)
          (backward-word)
          (ove-emacs-get #'(lambda () (interactive)
                             (forward-word)
                             (and (char-equal (char-after) ? )
                                  (forward-char))) "aw")))
  ("ew" (progn
          (forward-word)
          (backward-word)
          (ove-emacs-get #'forward-word "ew")))
  ("as" (progn
          (forward-sexp)
          (backward-sexp)
          (ove-emacs-get #'(lambda () (interactive)
                             (forward-sexp)
                             (and (char-equal (char-after) ? )
                                  (forward-char))) "as")))
  ("es" (progn
          (forward-sexp)
          (backward-sexp)
          (ove-emacs-get #'forward-sexp "es")))
  ("aS" (progn
          (backward-sentence)
          (ove-emacs-get #'forward-sentence "aS")))
  ("aP" (progn
          (backward-paragraph)
          (ove-emacs-get #'forward-paragraph "aP")))
  ("a'" (progn
          (zerolee-search-forward-char -1 ?')
          (ove-emacs-get '(lambda () (interactive)
                            (zerolee-search-forward-char 2 ?\')) "a'")))
  ("a<" (progn
          (zerolee-search-forward-char -1 ?<)
          (ove-emacs-get '(lambda () (interactive)
                            (let ((flag 1))
                              (forward-char 1)
                              (while (> flag 0)
                                (cond ((char-equal (char-after (point)) ?>) (setq flag (1- flag)))
                                      ((char-equal (char-after (point)) ?<) (setq flag (1+ flag))))
                                (forward-char 1)))) "a>")))
  ("at" (progn
          (web-mode-element-beginning)
          (ove-emacs-get #'web-mode-element-end "at")))
  ("e'" (progn
          (zerolee-search-forward-char -1 ?')
          (forward-char 1)
          (ove-emacs-get '(lambda () (interactive)
                            (zerolee-search-forward-char 1 ?\')
                            (backward-char 1)) "e'")))
  ("e<" (progn
          (zerolee-search-forward-char -1 ?<)
          (forward-char 1)
          (ove-emacs-get '(lambda () (interactive)
                            (let ((flag 1))
                              (while (> flag 0)
                                (cond ((char-equal (char-after (point)) ?>) (setq flag (1- flag)))
                                      ((char-equal (char-after (point)) ?<) (setq flag (1+ flag))))
                                (forward-char 1)))
                            (backward-char 1)) "e>")))

  ("et" (progn
          (web-mode-element-beginning)
          (web-mode-tag-end)
          (ove-emacs-get '(lambda () (interactive)
                            (web-mode-element-end)
                            (backward-char 1)
                            (web-mode-tag-beginning)) "at")))
  ("el" (progn
          (paredit-backward-up)
          (forward-char 1)
          (ove-emacs-get '(lambda () (interactive)
                            (paredit-forward-up)
                            (backward-char 1)) "el")))
  ("l" (progn
         (paredit-backward-up)
         (ove-emacs-get #'forward-sexp "s")))
  ("d" (progn
         (end-of-defun)
         (beginning-of-defun)
         (ove-emacs-get #'forward-sexp "s")))
  ("S" (ove-emacs-get #'forward-sentence "S"))
  ("P" (ove-emacs-get #'forward-paragraph "P"))
  ("w" (ove-emacs-get #'forward-word "w"))
  ("s" (ove-emacs-get #'forward-sexp "s"))
  (";" (ove-emacs-get #'end-of-line ";"))
  ("n" (let ((current-prefix-arg (1+ (prefix-numeric-value current-prefix-arg))))
         (ove-emacs-get #'end-of-line "n")))
  ("p" (let ((current-prefix-arg (- 1  (prefix-numeric-value current-prefix-arg))))
         (ove-emacs-get #'beginning-of-line "p")))
  ("c" (ove-emacs-get #'end-of-line "c"))
  ("k" (ove-emacs-get #'end-of-line "k"))
  ("m" (ove-emacs-get #'end-of-line "m"))
  ("t" (ove-emacs-get #'zerolee-search-forward-char "t")))

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
