;;; -*- lexical-binding: t; -*-
;; shift the meaning of C-s and C-M-s
;; shift the meaning of M-% and C-M-%
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

(global-set-key (kbd "C-M-]") 'up-list)
;; just-one-space
(global-set-key (kbd "<M-backspace>") 'just-one-space)

;; hippie-expand can expands almost anything. This include Lisp function names,
;; filenames from the hard disk, and text from the buffer.
(global-set-key (kbd "s-SPC") 'hippie-expand)

;; input-method
;; 取消掉默认的输入法快捷键
(global-unset-key (kbd "C-\\"))

;;; goto-last-change
(global-set-key (kbd "C-.")
                #'(lambda () (interactive)
                    (when (and buffer-undo-list
                               (listp buffer-undo-list))
                      (xref--push-markers)
                      (goto-char (last (cadr buffer-undo-list) 0)))))

;; electric-newline-and-maybe-indent
(global-set-key (kbd "C-j") 'newline-and-indent)

;; winner-mode
;; 主要用来撤销动作的
(setq winner-dont-bind-my-keys t)
(winner-mode t)
(global-set-key (kbd "C-x /") 'winner-undo)
(global-set-key (kbd "C-x s-r") 'winner-redo)

(global-set-key (kbd "s-r") 'undo-redo)

(global-set-key (kbd "C-c a") 'org-agenda)

;;; hydra
(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "?") #'hydra-info/body))
(global-set-key (kbd "M-<SPC>")
                #'(lambda ()
                    (interactive)
                    (call-process "fcitx-remote" nil nil nil "-c")
                    (hydra-f1/body)))
(global-set-key (kbd "<C-M-backspace>") #'backward-kill-sexp)

;;;
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)

;;;
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-H") 'windmove-swap-states-left)
(global-set-key (kbd "s-L") 'windmove-swap-states-right)
(global-set-key (kbd "s-J") 'windmove-swap-states-down)
(global-set-key (kbd "s-K") 'windmove-swap-states-up)


;;; dired
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "/")
    #'(lambda ()
        (interactive)
        (call-interactively #'dired-mark-files-regexp)
        (command-execute "tk"))))

;;; flymake
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-g p") #'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "M-g n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-g l")
    #'(lambda ()
        (interactive)
        (let ((buffer
               (format "*Flymake diagnostics for %s*" (current-buffer))))
          (if (get-buffer-window buffer)
              (delete-windows-on buffer)
            (flymake-show-diagnostics-buffer))))))

;;; eshell
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-d")
                #'(lambda (arg)
                    "Delete a character or quit eshell if there's nothing to delete."
                    (interactive "p")
                    (if (and (eolp) (looking-back eshell-prompt-regexp nil))
                        (eshell-life-is-too-much)
                      (delete-char arg))))))

;;; emacs-lisp
(define-key lisp-interaction-mode-map
  (kbd "C-c <return>") #'emacs-lisp-macroexpand)
(define-key emacs-lisp-mode-map (kbd "C-c <return>") #'emacs-lisp-macroexpand)
