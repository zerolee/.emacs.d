(defhydra hydra-esc (:color pink
			    :hint nil)
  "
   _i_: ivy           _f_: counsel-find-file
   _y_: yasnippet     _b_: ivy-switch-buffer
   _v_: vim           _s_: save-buffer
   _p_: paredit       _h_: backwardchar
                    _j_: nextline
                    _k_: previousline  
                    _l_: forwardchar
                    _d_: dired
  "
  ("i" hydra-counsel/body :exit t)
  ("y" hydra-yasnippet/body :exit t)
  ("v" hydra-vim/body :exit t)
  ("p" hydra-paredit/body :exit t)
  ("f" counsel-find-file :exit t)
  ("b" ivy-switch-buffer :exit t)
  ("s" save-buffer :exit t)
  ("j" next-line)
  ("k" previous-line)
  ("h" backward-char)
  ("l" forward-char)
  ("d" lzl-dired)
  ("c" nil "cancel")
  ("q" quit-window "quit" :color blue))

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
  ("q" quit-window "quit" :color blue))

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
  ("q" quit-window "quit" :color blue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-paredit (:color pink
				:hint nil)
  "
                      paredit
   -------------------------------------------------------------
   _r_: 去掉外层代码            _s_: (f (x b) l) => (f x b l)
   _(_: 吃掉左边的 s-exp        _)_: 吃掉右边的 s-exp
   _{_: 吐出左边的 s-exp        _}_: 吐出右边的 s-exp
   _S_: (he wo)=>  (he) (wo)  _J_: 将其重新连接起来
   _j_: nextline              _k_: previousline  
   _h_: backwardchar          _l_: forwardchar 
  "
  ("j" next-line)
  ("k" previous-line)
  ("h" backward-char)
  ("l" forward-char)
  ("r" paredit-raise-sexp)
  ("(" paredit-backward-slurp-sexp)
  (")" paredit-forward-slurp-sexp)
  ("{" paredit-backward-barf-sexp)
  ("}" paredit-forward-barf-sexp)
  ("S" paredit-split-sexp)
  ("J" paredit-join-sexps)
  ("s" paredit-splice-sexp)
  ("i" nil "cancel")
  ("q" quit-window "quit" :color blue))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lzlvim-y (beg end &optional region)
  (interactive (list (mark) (point)
		     (prefix-numeric-value current-prefix-arg)))
  (if (region-active-p)
      (kill-ring-save beg end region)
    (save-excursion
      (lzlvim-yy))))


(defhydra hydra-vim (:color pink
			    :hint nil)
  "
                              VIM
   -------------------------------------------------------------
   _j_: nextline          _k_: previousline  _I_: beginning-of-line
   _h_: backwardchar      _l_: forwardchar   _0_: beginning-of-line
   _x_: deletechar        _u_: undo          _d_: kill-line        
   _o_: vim-o             _O_: vim-O         _A_: end-of-line
   _v_: set-mark-command  _p_: vim-p         _$_: end-of-line
   _y_: kill-ring-save    _s_: save-buffer   _e_: eval-last-sexp
   _/_: Isearch
  "
  ("j" next-line)
  ("k" previous-line)
  ("h" backward-char)
  ("l" forward-char)
  ("x" delete-char)
  ("u" undo)
  ("o" lzlvim-o :exit t)
  ("O" lzlvim-O :exit t)
  ("v" set-mark-command)
  ("p" lzlvim-p)
  ("y" lzlvim-y)
  ("s" save-buffer)
  ("I" move-beginning-of-line :exit t)
  ("A" move-end-of-line :exit t)
  ("0" move-beginning-of-line)
  ("$" move-end-of-line)
  ("a" forward-char :exit t)
  ("d" kill-line)
  ("/" isearch-forward-regexp :exit t)
  ("e" eval-last-sexp)
  ("i" nil "cancel")
  ("q" quit-window "quit" :color blue))

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
