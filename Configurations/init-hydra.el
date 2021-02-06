;;; init-hydra.el --- hydra 相关配置 -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'hydra)

(defun zerolee-goto-ibuffer ()
  "打开并跳转到 Ibuffer."
  (interactive)
  (require 'zerolee-lib)
  (require 'ibuffer)
  (progn
    (ibuffer-list-buffers)
    (zerolee-goto-some-window "*Ibuffer*")))

(defhydra hydra-f1 (:color teal
                           :hint nil)
  "
   _l_: locate  _SPC_: shell          _o_: open-with
   _a_: ag      _r_: rg               _k_: delete-window
   _z_: zff     _i_: imenu            _M_: emms-favourite
   _v_: magit   _s_: swap-window      _j_: jump-window
  "
  ("b" ivy-switch-buffer)
  ("B" zerolee-goto-ibuffer)
  ("f" counsel-find-file)
  ("l" counsel-locate)
  ("a" counsel-ag)
  ("z" zerolee-find-file)
  ("o" zerolee-open-with)
  ("i" counsel-imenu)
  ("v" magit)
  ("r" zerolee-rg)
  ("d" (dired default-directory))
  ("m" zerolee-emms-default)
  ("M" zerolee-emms-favourite)
  ("s" ace-swap-window)
  ("j" ace-select-window)
  ("k" zerolee-delete-window)
  ("c" zerolee-compile)
  ("SPC" zerolee-eshell)
  ("<f1>" nil)
  ("M-<SPC>" nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-info (:color red
                             :hint nil)
  "
  _g_oto             _l_ast (←)      _\\^_up (↑)
  _i_dex item        _r_eturn (→)    _m_enu (↓)
  virtual _I_ndex    _H_istory       _,_next index item
 "
  ("l"   Info-history-back)
  ("r"   Info-history-forward)
  ("H"   Info-history)

  ("^"   Info-up)
  ("m"   Info-menu)
  ("g"   Info-goto-node)

  ("i"   Info-index)
  (","   Info-index-next)
  ("I"   Info-virtual-index)

  ("?"   Info-summary "Info summary")
  ("h"   Info-help "Info help")
  ("C-g" nil "cancel" :color blue))

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

(provide 'init-hydra)
;;; init-hydra.el ends here
