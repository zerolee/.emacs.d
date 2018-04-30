;; Keeping Points in Buffers for Current Session
(global-set-key (kbd "s-.") 'lzl-push-mark-to-ring)
(global-set-key (kbd "s-,") 'lzl-get-mark-from-ring)
(global-set-key (kbd "s-/") 'lzl-show-all-mark-in-ring)

;; shift the meaning of C-s and C-M-s
;; shift the meaning of M-% and C-M-%
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; just-one-space
(global-set-key (kbd "<M-backspace>") 'just-one-space)

;; hippie-expand can expands almost anything. This include Lisp function names,
;; filenames from the hard disk, and text from the buffer.
(global-set-key (kbd "s-SPC") 'hippie-expand)

;; Compile
(global-set-key (kbd "<C-f9>") 'compile)

;; input-method
;; 取消掉默认的输入法快捷键
(global-unset-key (kbd "C-\\"))


;; electric-newline-and-maybe-indent
(global-set-key (kbd "C-j") 'newline-and-indent)


;; remeber
(global-set-key (kbd "<C-f5>") 'remember)


;; windmove
(windmove-default-keybindings 'super)
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
(global-set-key (kbd "C-M-s") 'counsel-grep-or-swiper)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(with-eval-after-load 'counsel
  (define-key counsel-find-file-map (kbd "C-l") 'counsel-up-directory))

(global-set-key (kbd "C-c a") 'org-agenda)

;;; hydra
(global-set-key (kbd "<escape>") 'hydra-esc/body)
(define-key Info-mode-map (kbd "?") #'hydra-info/body)
(global-set-key (kbd "<f1>") #'hydra-f1/body)
(global-set-key (kbd "M-<SPC>") #'hydra-f1/body)



;;; 调节声音
(define-key exwm-mode-map (kbd "<XF86AudioLowerVolume>")
  (lambda () (interactive) (shell-command "amixer set Master 5%- &> /dev/null")))

(define-key exwm-mode-map (kbd "<XF86AudioRaiseVolume>")
  (lambda () (interactive) (shell-command "amixer set Master 5%+ &> /dev/null")))

(define-key exwm-mode-map (kbd "<XF86AudioMute>")
  (lambda () (interactive) (shell-command "amixer set Master 1+ toggle &> /dev/null")))
(define-key exwm-mode-map (kbd "s-h") 'windmove-left)
(define-key exwm-mode-map (kbd "s-l") 'windmove-right)
(define-key exwm-mode-map (kbd "s-j") 'windmove-down)
(define-key exwm-mode-map (kbd "s-k") 'windmove-up)

;;;
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)

(global-set-key (kbd "<f2>") 'treemacs-toggle)
(define-key treemacs-mode-map (kbd "m")
  '(lambda () (interactive)
     (let ((bname (buffer-name)))
       (treemacs-RET-action)
       (or (string-equal bname (buffer-name)) (other-window -1)))))

(global-set-key (kbd "C-=") 'er/expand-region)

;;; lsp
(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map [remap xref-find-apropos] #'lsp-ui-peek-find-workspace-symbol))

(global-set-key (kbd "S-<f2>") #'lsp-rename)

;;; gdb
(add-hook 'gdb-mode-hook '(lambda ()
                            (define-key c-mode-base-map (kbd "<f5>") #'gud-go)
                            (define-key c-mode-base-map (kbd "<f7>") #'gud-step)
                            (define-key c-mode-base-map (kbd "<f8>") #'gud-next)))

;;; iedit
(global-set-key (kbd "M-i") #'iedit-mode)

;;; avy
(global-set-key (kbd "M-g 1") 'avy-goto-char)
(global-set-key (kbd "M-g 2") 'avy-goto-char-2)
(global-set-key (kbd "M-g t") 'avy-goto-char-timer)
(global-set-key (kbd "M-g f") 'avy-goto-char-in-line)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g s") 'avy-goto-symbol-1)
(global-set-key (kbd "M-g 0") 'avy-goto-word-0)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
