;;; ove.el ---  vim edit style of own emacs style-*- lexical-binding: t; -*-

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

;;; Code:

(defun ove-ckm (which-ckm)
  (setq lzl-arg1 which-ckm)
  (if (string-equal which-ckm "m")
      (setq lzl-kill-or-save #'kill-ring-save)
    (setq lzl-kill-or-save #'kill-region))
  (hydra-emacs/ckm/body)
  (setq lzl-esc nil))

;;; ###autoload
(define-minor-mode ove-mode
  "拥有 vim 模式的 Emacs 风格的 minor"
  :init-value nil
  :lighter ""
  :keymap (make-sparse-keymap)
  (if (not ove-mode) (setq cursor-type 'bar)
    (setq cursor-type 'box))
  (overwrite-mode 0))



(define-key ove-mode-map (kbd "a") 'beginning-of-line)
(define-key ove-mode-map (kbd "C-a") '(lambda ()
                                        (interactive)
                                        (beginning-of-line)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "b") 'backward-char)
(define-key ove-mode-map (kbd "C-b") '(lambda ()
                                        (interactive)
                                        (backward-char)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "c") '(lambda ()
                                      (interactive)
                                      (ove-ckm "c")
                                      (ove-mode 0)))
(define-key ove-mode-map (kbd "d") 'delete-char)
(define-key ove-mode-map (kbd "C-d") '(lambda ()
                                        (interactive)
                                        (delete-char)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "e") 'move-end-of-line)
(define-key ove-mode-map (kbd "C-e") '(lambda ()
                                        (interactive)
                                        (move-end-of-line 1)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "f") 'forward-char)
(define-key ove-mode-map (kbd "C-f") '(lambda ()
                                        (interactive)
                                        (forward-char)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "g") 'goto-line)
(define-key ove-mode-map (kbd "h") 'paredit-backward)
(define-key ove-mode-map (kbd "H") 'delete-indentation)
(define-key ove-mode-map (kbd "i") 'ove-mode)
(define-key ove-mode-map (kbd "I") 'beginning-of-line-text)
(define-key ove-mode-map (kbd "j") 'forward-to-indentation)
(define-key ove-mode-map (kbd "C-j") '(lambda ()
                                        (interactive)
                                        (newline-and-indent)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "k") '(lambda ()
                                      (interactive)
                                      (ove-ckm "k")))
(define-key ove-mode-map (kbd "l") 'paredit-forward)
(define-key ove-mode-map (kbd "L") '(lambda ()
                                      (interactive)
                                      (delete-indentation 1)))
(define-key ove-mode-map (kbd "m") '(lambda ()
                                      (interactive)
                                      (ove-ckm "m")))
(define-key ove-mode-map (kbd "M") '(lambda ()
                                      (interactive)
                                      (save-excursion
                                        (call-interactively #'mark-whole-buffer)
                                        (mytab)
                                        (whitespace-cleanup)
                                        (call-interactively #'untabify))))
(define-key ove-mode-map (kbd "n") 'next-line)
(define-key ove-mode-map (kbd "C-n") '(lambda ()
                                        (interactive)
                                        (next-line)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "N") '(lambda ()
                                      (interactive)
                                      (save-excursion
                                        (end-of-line)
                                        (open-line 1))))
(define-key ove-mode-map (kbd "M-n") '(lambda ()
                                        (interactive)
                                        (end-of-line)
                                        (newline-and-indent)
                                        (yank)))
(define-key ove-mode-map (kbd "o") '(lambda ()
                                      (interactive)
                                      (end-of-line)
                                      (newline-and-indent)
                                      (ove-mode 0)))
(define-key ove-mode-map (kbd "O") '(lambda ()
                                      (interactive)
                                      (call-interactively #'ove-mode)
                                      (beginning-of-line)
                                      (open-line 1)
                                      (mytab)))
(define-key ove-mode-map (kbd "p") 'previous-line)
(define-key ove-mode-map (kbd "P") '(lambda ()
                                      (interactive)
                                      (save-excursion
                                        (beginning-of-line)
                                        (open-line 1))))
(define-key ove-mode-map (kbd "M-p") '(lambda ()
                                        (interactive)
                                        (beginning-of-line)
                                        (open-line 1)
                                        (mytab)
                                        (yank)))
(define-key ove-mode-map (kbd "q") '(lambda ()
                                      (interactive)
                                      (kill-buffer (current-buffer))))
(define-key ove-mode-map (kbd "r") '(lambda (arg char)
                                      (interactive "*p\nc")
                                      (delete-char 1)
                                      (insert-char char)))
(define-key ove-mode-map (kbd "R") '(lambda ()
                                      (interactive)
                                      (ove-mode 0)
                                      (setq cursor-type 'box)
                                      (overwrite-mode 1)))
;; (define-key ove-mode-map (kbd "s") 'backward-char)
;; (define-key ove-mode-map (kbd "t") 'backward-char)
(define-key ove-mode-map (kbd "u") 'undo)
(define-key ove-mode-map (kbd "v") 'scroll-up-command)
(define-key ove-mode-map (kbd "w") 'forward-to-word)
;; (define-key ove-mode-map (kbd "x") 'backward-char)
(define-key ove-mode-map (kbd "y") 'yank)
(define-key ove-mode-map (kbd "z") 'save-buffer)
(define-key ove-mode-map (kbd "Z") 'save-buffers-kill-terminal)
(define-key ove-mode-map (kbd "{") 'shrink-window-horizontally)
(define-key ove-mode-map (kbd "}") 'enlarge-window-horizontally)
(define-key ove-mode-map (kbd "^") 'enlarge-window)
(define-key ove-mode-map (kbd "(") 'paredit-backward-slurp-sexp)
(define-key ove-mode-map (kbd ")") 'paredit-forward-slurp-sexp)
(define-key ove-mode-map (kbd "<") 'paredit-backward-barf-sexp)
(define-key ove-mode-map (kbd ">") 'paredit-forward-barf-sexp)
(define-key ove-mode-map (kbd "S") 'paredit-split-sexp)
(define-key ove-mode-map (kbd "J") 'paredit-join-sexps)
(define-key ove-mode-map (kbd "M-<up>") 'paredit-splice-sexp)
(define-key ove-mode-map (kbd "<up>") '(lambda ()
                                         (interactive)
                                         (paredit-backward)
                                         (paredit-raise-sexp)))
(define-key ove-mode-map (kbd "<down>") 'paredit-raise-sexp)
(define-key ove-mode-map (kbd "<left>") 'paredit-splice-sexp-killing-forward)
(define-key ove-mode-map (kbd "<right>") 'paredit-splice-sexp-killing-backward)
(define-key ove-mode-map (kbd "'") 'eval-last-sexp)
(define-key ove-mode-map (kbd ".") 'repeat)
(define-key ove-mode-map (kbd "1") '(lambda () (interactive) (setq prefix-arg  1 )))
(define-key ove-mode-map (kbd "2") '(lambda () (interactive) (setq prefix-arg  2 )))
(define-key ove-mode-map (kbd "3") '(lambda () (interactive) (setq prefix-arg  3 )))
(define-key ove-mode-map (kbd "4") '(lambda () (interactive) (setq prefix-arg  4 )))
(define-key ove-mode-map (kbd "5") '(lambda () (interactive) (setq prefix-arg  5 )))
(define-key ove-mode-map (kbd "6") '(lambda () (interactive) (setq prefix-arg  6 )))
(define-key ove-mode-map (kbd "7") '(lambda () (interactive) (setq prefix-arg  7 )))
(define-key ove-mode-map (kbd "8") '(lambda () (interactive) (setq prefix-arg  8 )))
(define-key ove-mode-map (kbd "9") '(lambda () (interactive) (setq prefix-arg  9 )))
(define-key ove-mode-map (kbd "0") '(lambda () (interactive) (setq prefix-arg  0 )))

(provide 'ove)
;;; save-position.el ends here
