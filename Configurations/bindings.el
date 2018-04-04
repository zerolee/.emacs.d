;; Keeping Points in Buffers for Current Session
(global-set-key (kbd "s-.") 'lzl-push-mark-to-ring)
(global-set-key (kbd "s-,") 'lzl-get-mark-from-ring)
(global-set-key (kbd "s-?") 'lzl-show-all-mark-in-ring)

;; shift the meaning of C-s and C-M-s
;; shift the meaning of M-% and C-M-%
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
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

;; C-return
;; I use it to instead C-o
(global-set-key (kbd "<C-return>") 'open-line)


;; input-method
;; 取消掉默认的输入法快捷键
(global-unset-key (kbd "C-\\"))


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
(global-set-key (kbd "M-y")     'counsel-yank-pop)
(global-set-key (kbd "C-x b")   'ivy-switch-buffer)


(global-set-key (kbd "C-h f")   'counsel-describe-function)
(global-set-key (kbd "C-h v")   'counsel-describe-variable)
(global-set-key (kbd "C-h S")   'counsel-info-lookup-symbol)
(global-set-key (kbd "C-h a")   'counsel-apropos)

;;(global-set-key (kbd "M-w")   'ivy-kill-ring-save)


;; C-c C-o ivy-occur, C-x C-q 开始编辑, C-x C-s 保存编辑， C-c C-c 退出
(global-set-key (kbd "C-M-s") 'swiper)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(with-eval-after-load 'counsel
  (define-key counsel-find-file-map (kbd "C-l") 'counsel-up-directory))

(global-set-key (kbd "C-c a") 'org-agenda)

;;; hydra
(global-set-key (kbd "<escape>") 'hydra-esc/body)
(define-key Info-mode-map (kbd "?") #'hydra-info/body)



;;; 键盘宏
(global-set-key (kbd "C-o")   'lzl-cool-newline)
(global-set-key (kbd "C-x d") 'lzl-dired)


;;;
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-m") 'downcase-word)
