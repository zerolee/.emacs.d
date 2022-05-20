;;; init-hydra.el --- hydra 相关配置 -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'hydra)

(defhydra hydra-f1 (:color teal
                           :hint nil)
  "
   _l_: locate  _SPC_: shell          _o_: open-with
   _r_: rg      _k_: delete-window    _v_: magit
   _z_: zff     _i_: imenu            _j_: jump-window
   _u_: ctags   _s_: swap-window
  "
  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("l" counsel-locate)
  ("z" zerolee-find-file)
  ("o" zerolee-open-with)
  ("i" counsel-imenu)
  ("v" magit)
  ("r" zerolee-rg)
  ("d" (dired default-directory))
  ("m" zerolee-emms-default)
  ("s" ace-swap-window)
  ("j" ace-select-window)
  ("k" zerolee-delete-window)
  ("c" zerolee-compile)
  ("p" citre-peek)
  ("u" zerolee-regenerate-ctags)
  ("SPC" zerolee-eshell)
  ("<f1>" nil)
  ("M-<SPC>" nil))

(provide 'init-hydra)
;;; init-hydra.el ends here
