;;; -*- lexical-binding: t; -*-
(require 'zerolee-lib)
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

(global-set-key (kbd "s-r") 'undo-redo)

(global-set-key (kbd "C-c a") 'org-agenda)

;;; hydra
(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "?") #'hydra-info/body))
(global-set-key (kbd "M-<SPC>") #'(lambda ()
                                    (interactive)
                                    (shell-command "fcitx-remote -c")
                                    (hydra-f1/body)))
(global-set-key (kbd "<C-M-backspace>") #'backward-kill-sexp)
(global-set-key (kbd "C-w") '(lambda () (interactive)
                               (if (use-region-p)
                                   (kill-region (region-beginning) (region-end))
                                 (ove-ckm "c"))))
(global-set-key (kbd "M-w") '(lambda () (interactive)
                               (if (use-region-p)
                                   (kill-ring-save (region-beginning) (region-end))
                                 (ove-ckm "m"))))

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
               (get-buffer (format "*Flymake diagnostics for %s*" (current-buffer)))))
          (if (zerolee-position-some-window buffer)
              (delete-windows-on buffer)
            (flymake-show-diagnostics-buffer))))))

;;; emacs-lisp
(define-key lisp-interaction-mode-map
  (kbd "C-c <return>") #'emacs-lisp-macroexpand)
(define-key emacs-lisp-mode-map (kbd "C-c <return>") #'emacs-lisp-macroexpand)
