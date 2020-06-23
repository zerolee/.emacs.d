;;;  my-hydra-other.el ---  使用 hydra 让生活更美好 -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; 使用 hydra
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-info (:color red
                             :hint nil)
  "
  _]_ forward  (next logical node)       _l_ast (←)                 _\\^_up (↑)                        _f_ollow reference
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

  ("^"   Info-up)
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

(provide 'my-hydra-other)
;;; my-hydra-other.el ends here
