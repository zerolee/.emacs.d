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

;; just-one-space
(global-set-key (kbd "<M-backspace>") 'just-one-space)

;; There are two functions for going to the matching starting or
;; ending brace called forwarf-sexp and backward-sexp
(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)

;; Moving to a Line Specified by a Number
(global-set-key [(meta g)] 'goto-line)


;; hippie-expand can expands almost anything. This include Lisp function names,
;; filenames from the hard disk, and text from the buffer.
(global-set-key (kbd "s-SPC") 'hippie-expand)

;; Compile
(global-set-key (kbd "<C-f9>") 'compile)


;; Macro f3: start define macro f4:end define macro
(global-set-key (kbd "C-=") 'call-last-kbd-macro)

;; C-return
;; I use it to instead C-o
(global-set-key (kbd "<C-return>") 'open-line)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x")     'counsel-M-x)

(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h S") 'counsel-info-lookup-symbol)


(global-set-key (kbd "C-h a")   'counsel-apropos)
(global-set-key (kbd "M-y")     'counsel-yank-pop)
;;(global-set-key (kbd "M-w")   'ivy-kill-ring-save)

(global-set-key (kbd "C-x c l") 'counsel-locate)
(global-set-key (kbd "C-x c a") 'counsel-ag)
(global-set-key (kbd "C-x c f") 'counsel-fzf)
(global-set-key (kbd "C-x c i") 'counsel-imenu)
(global-set-key (kbd "C-x c g") 'counsel-git)

(global-set-key (kbd "C-x c p") 'ivy-push-view)

;; C-c C-o ivy-occur, C-x C-q 开始编辑, C-x C-s 保存编辑， C-c C-c 退出
(global-set-key (kbd "C-M-s") 'swiper)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key ivy-minibuffer-map (kbd "C-l") 'counsel-up-directory)

(global-set-key (kbd "C-c a") 'org-agenda)
