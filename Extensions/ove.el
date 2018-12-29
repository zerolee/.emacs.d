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

(defgroup ove-face nil
  "类 vim 模式的的 emacs 按键风格"
  :group 'ove-face)
(defface ove-aquamarine
  '((((min-colors 88) (background dark))
     (:background "aquamarine" :foreground "black"))
    (((background dark)) (:background "blue" :foreground "black"))
    (((min-colors 88)) (:background "aquamarine"))
    (t (:background "blue")))
  "Face for hi-lock mode."
  :group 'ove-face
  :version "27.1")

(let (lzl-arg1
      lzl-kill-or-save
      emacs-ckm-point)
  (defun ove-ckm (which-ckm)
    (setq lzl-arg1 which-ckm)
    (setq emacs-ckm-point (point))
    (if (string-equal which-ckm "m")
        (setq lzl-kill-or-save #'kill-ring-save)
      (setq lzl-kill-or-save #'kill-region))
    (hydra-emacs/ckm/body))

  (defun lzl-emacs-get (lzl-move lzl-arg2)
    "删除或者保存 region 中的数据"
    (if (string-match lzl-arg2 "<p")
        (end-of-line))
    (if (string-match lzl-arg2 ">nckm")
        (beginning-of-line))
    (let ((current-position (point)))
      (funcall lzl-kill-or-save current-position
               (progn
                 (call-interactively lzl-move)
                 (pulse-momentary-highlight-region current-position (point) 'ove-aquamarine)
                 (point))))
    ;; k
    (if (and (string-match lzl-arg1 "k")
             (string-match lzl-arg2 "<>npk"))
        (let ((pp (point)))
          (if (and  (search-forward "\n" nil  t 1)
                    (= (1+ pp) (point)))
              (delete-char -1))
          (goto-char pp)))
    ;; 如果复制的话，恢复其位置
    (if (string-equal lzl-arg1 "m")
        (goto-char emacs-ckm-point))))

(defsubst ove-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun ove-eval-sexp-dwim ()
  "如果当前所处位置是 list 或字符串或符号结尾则执行这个 sexp

   若果在字符串内，则假设字符串在 list 内，则执行整个 list
   如果在一个符号内，且这个符号以括号开头则跳出整个 list，
   否则执行这个符号"
  (interactive)
  (save-excursion
    (let ((state (ove-current-parse-state)))
      (if (equal last-command 'ove-eval-sexp-dwim)
          (end-of-defun)
        (if (nth 3 state)
            (up-list (1+ (nth 0 state)) t)
          (if (char-equal (char-before (point)) ?\()
              (up-list (nth 0 state))
            (progn
              (backward-sexp 1)
              (if (char-equal (char-before (point)) ?\()
                  (up-list (nth 0 state))
                (forward-sexp 1)))))))
    (my-eval-last-sexp)
    (setq this-command 'ove-eval-sexp-dwim)))

;;;###autoload
(define-minor-mode ove-mode
  "拥有 vim 模式的 Emacs 风格的 minor"
  :init-value nil
  :lighter ""
  :keymap (make-sparse-keymap)
  (if (not ove-mode) (setq cursor-type 'bar)
    (setq cursor-type 'box))
  (overwrite-mode 0))



(define-key ove-mode-map (kbd "a") 'beginning-of-line)
(define-key ove-mode-map (kbd "A") #'(lambda ()
                                       (interactive)
                                       (beginning-of-line)
                                       (forward-to-word 1)))
(define-key ove-mode-map (kbd "C-a") '(lambda ()
                                        (interactive)
                                        (beginning-of-line)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "b") 'backward-char)
(define-key ove-mode-map (kbd "C-b") #'(lambda ()
                                         (interactive)
                                         (ove-mode 0)))
(define-key ove-mode-map (kbd "c") '(lambda ()
                                      (interactive)
                                      (ove-ckm "c")
                                      (ove-mode 0)))
(define-key ove-mode-map (kbd "d") 'delete-char)
(define-key ove-mode-map (kbd "C-d") '(lambda ()
                                        (interactive)
                                        (delete-char 1)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "e") 'move-end-of-line)
(define-key ove-mode-map (kbd "C-e") '(lambda ()
                                        (interactive)
                                        (move-end-of-line 1)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "f") 'forward-char)
(define-key ove-mode-map (kbd "C-f") #'(lambda ()
                                         (interactive)
                                         (or (eolp)
                                             (forward-char))
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
(define-key ove-mode-map (kbd "Q") 'kill-buffer-and-window)
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
(define-key ove-mode-map (kbd "[") 'paredit-add-to-previous-list)
(define-key ove-mode-map (kbd "]") 'paredit-add-to-next-list)
(define-key ove-mode-map (kbd "<") 'paredit-backward-barf-sexp)
(define-key ove-mode-map (kbd ">") 'paredit-forward-barf-sexp)
(define-key ove-mode-map (kbd "S") 'paredit-split-sexp)
(define-key ove-mode-map (kbd "J") 'paredit-join-sexps)
(define-key ove-mode-map (kbd "<up>") '(lambda ()
                                         (interactive)
                                         (paredit-backward)
                                         (paredit-raise-sexp)))
(define-key ove-mode-map (kbd "<down>") 'paredit-raise-sexp)
(define-key ove-mode-map (kbd "<left>") 'paredit-splice-sexp-killing-forward)
(define-key ove-mode-map (kbd "<right>") 'paredit-splice-sexp-killing-backward)
(define-key ove-mode-map (kbd ";") 'ove-eval-sexp-dwim)
(define-key ove-mode-map (kbd ".") 'repeat)

(defmacro ove-set-prefix-arg (arg)
  `(define-key ove-mode-map (kbd ,arg)
     (lambda () (interactive)
       (if (numberp last-command)
           (setq prefix-arg (+ (* last-command 10) ,(string-to-number arg)))
         (setq prefix-arg ,(string-to-number arg)))
       (setq this-command prefix-arg))))

(ove-set-prefix-arg "0")
(ove-set-prefix-arg "1")
(ove-set-prefix-arg "2")
(ove-set-prefix-arg "3")
(ove-set-prefix-arg "4")
(ove-set-prefix-arg "5")
(ove-set-prefix-arg "6")
(ove-set-prefix-arg "7")
(ove-set-prefix-arg "8")
(ove-set-prefix-arg "9")

(provide 'ove)
;;; ove.el ends here
