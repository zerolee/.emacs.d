;;; -*- lexical-binding: t; -*-
(require 'zerolee-lib)
(require 'my-shell)
(defhydra hydra-f1 (:color teal
                           :hint nil)
  "
   _l_: locate  _p_: ivy-push-view    _y_: yasnippet
   _a_: ag      _P_: ivy-pop-view     _k_: delete-window
   _z_: zff     _i_: imenu            _M_: emms-favourite
   _v_: magit   _s_: swap-window      _w_: select-window
   _r_: rg      _SPC_: shell          _c_: compile
  "
  ("b" ivy-switch-buffer)
  ("B" zerolee-goto-ibuffer)
  ("f" counsel-find-file)
  ("l" counsel-locate)
  ("a" counsel-ag)
  ("z" zerolee-find-file)
  ("i" counsel-imenu)
  ("v" magit)
  ("p" ivy-push-view)
  ("P" ivy-pop-view)
  ("y" company-yasnippet)
  ("r" zerolee-rg)
  ("d" (dired default-directory))
  ("m" zerolee-emms-default)
  ("M" zerolee-emms-favourite)
  ("s" ace-swap-window)
  ("w" ace-select-window)
  ("k" ace-delete-window)
  ("c" zerolee-compile)
  ("SPC" zerolee-eshell)
  ("<f1>" nil)
  ("M-<SPC>" nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydra-esc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
