;;; bindings.el --- 按键绑定相关 -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'zerolee-lib)
;; winner-mode
;; 主要用来撤销动作的
(setq winner-dont-bind-my-keys t)
(winner-mode t)

(zerolee-set-key
 ("C-\\" nil) ;; 取消掉默认的输入法快捷键
 ("C-s" #'isearch-forward-regexp)
 ("C-r" #'isearch-backward-regexp)
 ("C-M-r" #'isearch-backward)
 ("M-%" #'query-replace-regexp)
 ("C-M-%" #'query-replace)
 ("C-M-]" #'up-list)
 ("s-SPC" #'hippie-expand)
 ("C-." #'zerolee-goto-last-edit)
 ("C-j" #'newline-and-indent)
 ("s-r" #'undo-redo)
 ("C-c a" #'org-agenda)
 ("<C-M-backspace>" #'backward-kill-sexp)
 ("C-x /" #'winner-undo)
 ("C-x s-r" #'winner-redo)
 ("M-0" #'delete-window)
 ("M-1" #'delete-other-windows)
 ("M-2" #'split-window-below)
 ("M-3" #'split-window-right)
 ("s-h" #'windmove-left)
 ("s-l" #'windmove-right)
 ("s-j" #'windmove-down)
 ("s-k" #'windmove-up)
 ("s-H" #'windmove-swap-states-left)
 ("s-L" #'windmove-swap-states-right)
 ("s-J" #'windmove-swap-states-down)
 ("s-K" #'windmove-swap-states-up)
 ("S-<up>" #'enlarge-window)
 ("S-<down>" #'shrink-window)
 ("S-<left>" #'shrink-window-horizontally)
 ("S-<right>" #'enlarge-window-horizontally)
 lisp-interaction-mode-map
 emacs-lisp-mode-map
 ("C-c <return>" #'emacs-lisp-macroexpand))

;;; hydra
(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "?") #'hydra-info/body))
(global-set-key (kbd "M-<SPC>")
                (lambda ()
                  (interactive)
                  (zerolee-ime-disable)
                  (hydra-f1/body)))

;;; flymake
(with-eval-after-load 'flymake
  (zerolee-set-key
   flymake-mode-map
   ("M-g p" #'flymake-goto-prev-error)
   ("M-g n" #'flymake-goto-next-error)
   ("M-g l" (lambda () (interactive)
              (let ((buffer (flymake--diagnostics-buffer-name)))
                (if (get-buffer-window buffer)
                    (delete-windows-on buffer)
                  (flymake-show-buffer-diagnostics)))))))

;;; key-chord "df"
(global-set-key [key-chord ?d ?f] (lambda () (interactive) (vesie-mode 1)))
(setq input-method-function
      (lambda (first-char)
        (if (memq first-char '(?d ?f))
            (let ((input-method-function nil)
                  (next-char (read-event nil nil 0.05)))
              (if (and (memq next-char '(?d ?f))
                       (not (eq first-char next-char)))
                  (list 'key-chord ?d ?f)
                (and next-char (push next-char unread-command-events))
                (list first-char)))
          (list first-char))))

(provide 'bindings)
;;; bindings.el ends here
