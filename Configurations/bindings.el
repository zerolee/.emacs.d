(message "Loading bindings...")
;; You can used C-- to inverse C-x o.
;;(global-set-key [(control kp-subtract)] 'sams-other-window-backwards) 
;;(global-set-key (kbd "<C-M-left>") 'sams-other-window-backwards)
;;(global-set-key (kbd "<C-M-right>") 'other-window)

;; Keeping Points in Buffers for Current Session
(global-set-key (kbd "s-.") 'sams-cm-save-point)
(global-set-key (kbd "s-,") 'sams-cm-rotate)

;; shift the meaning of C-s and C-M-s
;; shift the meaning of M-% and C-M-%
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; use M-SPC instead C-@ or C-SPC
;; (global-set-key (kbd "M-SPC") 'set-mark-command)

;; just-one-space
(global-set-key (kbd "<M-backspace>") 'just-one-space)

;; There are two functions for going to the matching starting or
;; ending brace called forwarf-sexp and backward-sexp
(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)

;; Moving to a Line Specified by a Number
(global-set-key [(meta g)] 'goto-line)

;;;You Know!!! bindkey to win+r
;;(require 'redo)
;;(global-set-key [(s r)] 'redo)

;; hippie-expand can expands almost anything. This include Lisp function names,
;; filenames from the hard disk, and text from the buffer.
(global-set-key (kbd "s-SPC") 'hippie-expand)


;; Managing Bookmarks 
;;(global-set-key (kbd "<C-f3>") 'bookmark-set)
;;(global-set-key (kbd "<C-f4>") 'bookmark-jump)

;; Compile
(global-set-key (kbd "<C-f9>") 'compile)


;; Macro f3: start define macro f4:end define macro
(global-set-key (kbd "C-=") 'call-last-kbd-macro)

;; C-return
;; I use it to instead C-o
(global-set-key (kbd "<C-return>") 'open-line)

;; outline-minor-mode
;; 使用C-. 然后接着C-d隐藏一个子树C-i可以打开该子树 C-e显示一个entry
;; C-q可以隐藏全部，C-a可以显示全部
(setq outline-minor-mode-prefix [(control .)])

;; cscope
;;;(global-set-key (kbd "C-c s s") 'cscope-find-this-symbol)
;;;(global-set-key (kbd "C-c s g") 'cscope-find-global-definition)
;;;(global-set-key (kbd "C-c s G") 'cscope-find-global-definition-no-prompting)


;; global
;;;;(global-set-key (kbd "C-c g v") 'gtags-visit-rootdir) ;;找函数搜索的根目录
;;;;(global-set-key (kbd "C-c g t") 'gtags-find-tag-from-here) ;;无所不能者
;;;;(global-set-key (kbd "C-c g c") 'gtags-find-rtags) ;;找函数调用
;;;;(global-set-key (kbd "C-c g s") 'gtags-find-symbol) ;;变量的定义和调用
;;;;(global-set-key (kbd "C-c g g") 'gtags-find-with-grep) ;;在项目中进行搜索搜索
;;;;(global-set-key (kbd "C-c g f") 'gtags-find-file) ;;在项目中搜索文件

;; input-method
;; 取消掉默认的输入法快捷键
(global-unset-key (kbd "C-\\"))


;; yasnippet
(global-set-key (kbd "C-c y i") 'yas-insert-snippet)
(global-set-key (kbd "C-c y n") 'yas-new-snippet)
(global-set-key (kbd "C-c y e") 'yas-expand)


;; electric-newline-and-maybe-indent
(global-set-key (kbd "C-j") 'newline-and-indent)


;; remeber
(global-set-key (kbd "<C-f5>") 'remember)


;; windmove
(windmove-default-keybindings)
;;(global-set-key (kbd "<s-S-left>") 'windmove-left)

;; winner-mode
;; 主要用来撤销动作的
(setq winner-dont-bind-my-keys t)
(winner-mode t)
(global-set-key (kbd "C-x /") 'winner-undo)
(global-set-key (kbd "C-x s-r") 'winner-redo)



(message "Loading bindings...done")
