;; Keeping Points in Buffers for Current Session
(global-set-key (kbd "s-.") 'sp-push-position-to-ring)
(global-set-key (kbd "s-,") 'sp-get-position-from-ring)
(global-set-key (kbd "s-/") 'sp-show-all-position-in-ring)

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

;; winner-mode
;; 主要用来撤销动作的
(setq winner-dont-bind-my-keys t)
(winner-mode t)
(global-set-key (kbd "C-x /") 'winner-undo)
(global-set-key (kbd "C-x s-r") 'winner-redo)

(global-set-key (kbd "C-c a") 'org-agenda)

;;; hydra
(global-set-key (kbd "<escape>") 'hydra-esc/body)
(define-key Info-mode-map (kbd "?") #'hydra-info/body)
(global-set-key (kbd "<f1>") #'hydra-f1/body)
(global-set-key (kbd "M-<SPC>") #'hydra-f1/body)

;;;
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)



;;; lsp
(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map [remap xref-find-apropos] #'lsp-ui-peek-find-workspace-symbol))

(global-set-key (kbd "S-<f2>") #'lsp-rename)
(global-set-key (kbd "M-g l") #'flycheck-list-errors)


(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
