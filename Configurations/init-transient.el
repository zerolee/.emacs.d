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
;;; gud  相关 transient 的设置
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

(provide 'init-transient)
;;; init-transient.el ends here
