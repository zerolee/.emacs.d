;;;  init-transient.el ---  使用了 transient 的一些设置 -*- lexical-binding: t; -*-

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

;;; info 相关 transient 的设置
;;; 全局相关  transient 的设置
;;; Code:
(require 'transient)
(transient-define-prefix transient/my-info ()
  "info 中所使用的命令的小提示."
  [[("g"   "goto"            Info-goto-node)
    ("^"   "up(↑)"           Info-up)
    ("m"   "menu(↓)"         Info-menu)]
   [("i"   "index item"      Info-index)
    (","   "next index item" Info-index-next)
    ("I"   "virtual Index"   Info-virtual-index)]
   [("L"   "History"         Info-history)
    ("h"   "Info help"       Info-help)]])

(transient-define-prefix transient/my-menu ()
  "全局菜单."
  [[("1" "1"             digit-argument)
    ("2" "2"             digit-argument)
    ("3" "4"             digit-argument)
    ("4" "4"             digit-argument)]
   [("r" "rg"            zerolee-rg)
    ("z" "zff"           zerolee-find-file)
    ("m" "emms"          zerolee-emms-default)
    ("o" "open"          zerolee-open-with)]
   [("d" "dired"         dired-jump)
    ("v" "magit"         magit)
    ("i" "imenu"         counsel-imenu)
    ("l" "locate"        counsel-locate)]
   [("k" "delete-window" zerolee-delete-window)
    ("u" "update-tags"   zerolee-regenerate-ctags)
    ("j" "jump-window"   ace-select-window)
    ("b" "switch-buffer" ivy-switch-buffer)]
   [("SPC" "shell"       zerolee-eshell)
    ("f" "find-file"     counsel-find-file)
    ("s" "swap-window"   ace-swap-window)
    ("c" "compile"       zerolee-compile)
    ("M-SPC" "quit"      transient-quit-one)]])

(provide 'init-transient)
;;; init-transient.el ends here
