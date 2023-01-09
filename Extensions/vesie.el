;;; vesie.el ---  vim edit style in Emacs-*- lexical-binding: t; -*-

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
(require 'thunk)
(require 'treesit)
(require 'zerolee-lib)

(defgroup vesie-face nil
  "类 vim 模式的的 emacs 按键风格"
  :group 'vesie-face)
(defface vesie-aquamarine
  '((((min-colors 88) (background dark))
     (:background "aquamarine" :foreground "black"))
    (((background dark)) (:background "blue" :foreground "black"))
    (((min-colors 88)) (:background "aquamarine"))
    (t (:background "blue")))
  "Face for hi-lock mode."
  :group 'vesie-face
  :version "29.1")

(defvar vesie-emacs/ckm-map (make-sparse-keymap) "Keymap for ckm commands.")

(defsubst vesie--current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defsubst vesie--get-html-node ()
  "返回所需要的 NODE."
  (treesit-parser-create 'html)
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (parent)
     (and (member (treesit-node-type parent)
                  '("doctype" "element" "script_element" "style_element"))
          (/= (treesit-node-start (treesit-node-at (point)))
              (treesit-node-start parent))
          (/= (treesit-node-end (treesit-node-at (point)))
              (treesit-node-end parent))))))

(defun vesie--l-dwim (N)
  "N=1: el, N=0:l."
  (thunk-let ((state (vesie--current-parse-state))
              (node (vesie--get-html-node)))
    (cond ((thing-at-point 'url) (beginning-of-thing 'url))
          ((memq major-mode '(mhtml-mode html-ts-mode))
           (goto-char (treesit-node-start node)) (forward-sexp N))
          ((or (< 0 (car state)) (nth 3 state))
           (paredit-backward-up) (forward-char N)))
    (lambda () (interactive)
      (cond ((thing-at-point 'url) (end-of-thing 'url))
            ((memq major-mode '(mhtml-mode html-ts-mode))
             (goto-char (treesit-node-end node)) (backward-sexp N))
            ((or (< 0 (car state)) (nth 3 state))
             (if (= N 1) (paredit-forward-up) (forward-sexp))
             (backward-char N))))))

(let (vesie-arg1
      vesie-kill-or-save
      emacs-ckm-point
      cpa)
  (defun vesie-ckm (which-ckm)
    (setq vesie-arg1 which-ckm
          emacs-ckm-point (point)
          cpa current-prefix-arg)
    (if (string-equal which-ckm "m")
        (setq vesie-kill-or-save #'kill-ring-save)
      (setq vesie-kill-or-save #'kill-region))
    (set-transient-map vesie-emacs/ckm-map t))

  (defun vesie--emacs-get (vesie-move vesie-arg2)
    "删除或者保存 region 中的数据."
    (setq current-prefix-arg (prefix-numeric-value (or cpa current-prefix-arg)))
    (when (string= vesie-arg2 "p")
      (setq current-prefix-arg (- 1 current-prefix-arg)))
    (when (string= vesie-arg2 "n")
      (cl-incf current-prefix-arg))
    (let ((current-position (point)))
      (funcall vesie-kill-or-save current-position
               (progn
                 (call-interactively vesie-move)
                 (pulse-momentary-highlight-region
                  current-position (point) 'vesie-aquamarine)
                 (point))))
    ;; k 与 c 的区别
    (and (string= vesie-arg1 "k") (string-match vesie-arg2 "<>npk")
         (char-after) (delete-char 1))
    ;; 如果复制的话，恢复其位置
    (when (string-equal vesie-arg1 "m")
      (goto-char emacs-ckm-point))
    (setq overriding-terminal-local-map nil
          cpa nil)))

;;;###autoload
(define-minor-mode vesie-mode
  "拥有 vim 模式的 Emacs 风格的 minor"
  :init-value nil
  :lighter ""
  :keymap (make-sparse-keymap)
  (if (not vesie-mode) (setq cursor-type 'bar)
    (setq cursor-type 'box)
    (zerolee-ime-disable))
  (overwrite-mode 0))

(zerolee-set-key
  ("<escape>" (lambda () (interactive) (vesie-mode 1)))
  ("C-w" (lambda () (interactive)
           (if (use-region-p)
               (kill-region (region-beginning) (region-end))
             (vesie-ckm "c"))))
  ("M-w" (lambda () (interactive)
           (if (use-region-p)
               (kill-ring-save (region-beginning) (region-end))
             (vesie-ckm "m"))))
  vesie-mode-map
  ("a" #'beginning-of-line)
  ("C-a" (lambda () (interactive) (beginning-of-line) (vesie-mode 0)))
  ("b" #'backward-char)
  ("c" (lambda () (interactive) (vesie-ckm "c") (vesie-mode 0)))
  ("d" #'delete-char)
  ("e" #'move-end-of-line)
  ("C-e" (lambda () (interactive) (move-end-of-line 1) (vesie-mode 0)))
  ("f" #'forward-char)
  ("C-f" (lambda () (interactive) (or (eolp) (forward-char)) (vesie-mode 0)))
  ("g" #'goto-line)
  ("h" #'paredit-backward)
  ("H" (lambda () (interactive) (save-excursion (delete-indentation))))
  ("i" #'vesie-mode)
  ("I" #'beginning-of-line-text)
  ("j" (lambda () (interactive)
         (if (minibufferp (current-buffer))
             (call-interactively #'ivy-done)
           (call-interactively #'forward-to-indentation))))
  ("C-j" (lambda () (interactive) (newline-and-indent) (vesie-mode 0)))
  ("k" (lambda () (interactive) (vesie-ckm "k")))
  ("l" #'paredit-forward)
  ("L" (lambda () (interactive) (save-excursion (delete-indentation 1))))
  ("m" (lambda () (interactive) (vesie-ckm "m")))
  ("M" (lambda () (interactive)
         (save-excursion
           (call-interactively #'mark-whole-buffer)
           (call-interactively #'indent-for-tab-command)
           (whitespace-cleanup)
           (call-interactively #'untabify))))
  ("n" #'kmacro/my-next-line)
  ("N" (lambda () (interactive)
         (save-excursion
           (end-of-line)
           (open-line 1))))
  ("M-n" (lambda () (interactive)
           (save-excursion
             (end-of-line)
             (newline-and-indent)
             (yank))))
  ("o" (lambda () (interactive)
         (end-of-line) (newline-and-indent) (vesie-mode 0)))
  ("O" (lambda () (interactive)
         (call-interactively #'vesie-mode)
         (beginning-of-line)
         (open-line 1)
         (call-interactively #'indent-for-tab-command)))
  ("p" #'kmacro/my-prev-line)
  ("P" (lambda () (interactive)
         (save-excursion
           (beginning-of-line)
           (open-line 1))
         (when (bolp)
           (call-interactively #'next-line))))
  ("M-p" (lambda () (interactive)
           (save-excursion
             (beginning-of-line)
             (open-line 1)
             (call-interactively #'indent-for-tab-command)
             (yank))))
  ("q" (lambda () (interactive)
         (if (minibufferp (current-buffer))
             (call-interactively #'minibuffer-keyboard-quit)
           (kill-buffer)
           (when (< 1 (cl-count (current-buffer)
                                (mapcar #'window-buffer (window-list))))
             (delete-window)))))
  ("Q" #'kill-buffer-and-window)
  ("r" (lambda (char) (interactive "*c") (delete-char 1) (insert-char char)))
  ("R" (lambda () (interactive)
         (vesie-mode 0) (setq cursor-type 'box) (overwrite-mode 1)))
  ("s" #'avy-goto-char-in-line)
  ("t" #'avy-goto-char)
  ("u" #'undo)
  ("v" #'scroll-up-command)
  ("w" #'forward-to-word)
  ("x" (lambda () (interactive) (hs-minor-mode) (hs-toggle-hiding)))
  ("X" (lambda () (interactive)
         (if (bound-and-true-p hs-minor-mode)
             (hs-minor-mode -1)
           (hs-minor-mode)
           (hs-hide-all))))
  ("y" #'yank)
  ("z" #'save-buffer)
  ("Z" #'save-buffers-kill-terminal)
  ("(" #'paredit-backward-slurp-sexp)
  (")" #'paredit-forward-slurp-sexp)
  ("[" #'paredit-add-to-previous-list)
  ("]" #'paredit-add-to-next-list)
  ("<" #'paredit-backward-barf-sexp)
  (">" #'paredit-forward-barf-sexp)
  ("S" #'paredit-split-sexp)
  ("J" #'paredit-join-sexps)
  ("<up>" (lambda () (interactive) (paredit-backward) (paredit-raise-sexp)))
  ("<down>" #'paredit-raise-sexp)
  ("<left>" #'paredit-splice-sexp-killing-forward)
  ("<right>" #'paredit-splice-sexp-killing-backward)
  (";" #'kmacro/my-eval-last-sexp)
  ("." #'repeat)
  vesie-emacs/ckm-map
  ("<" (lambda () (interactive)
         (let ((current-prefix-arg (point-min)))
           (end-of-line)
           (vesie--emacs-get #'goto-char "<"))))
  (">" (lambda () (interactive)
         (let ((current-prefix-arg (point-max)))
           (beginning-of-line)
           (vesie--emacs-get #'goto-char ">"))))
  ("i" (lambda () (interactive) (vesie--emacs-get #'beginning-of-line "i")))
  ("aw" (lambda () (interactive)
          (beginning-of-thing 'word)
          (vesie--emacs-get (lambda () (interactive)
                              (forward-word)
                              (and (char-equal (char-after) ? )
                                   (forward-char))) "aw")))
  ("ew" (lambda () (interactive)
          (beginning-of-thing 'word) (vesie--emacs-get #'forward-word "ew")))
  ("as" (lambda () (interactive)
          (beginning-of-thing 'sexp)
          (vesie--emacs-get (lambda () (interactive)
                              (forward-sexp)
                              (and (char-equal (char-after) ? )
                                   (forward-char))) "as")))
  ("es" (lambda () (interactive)
          (beginning-of-thing 'sexp)
          (vesie--emacs-get #'forward-sexp "es")))
  ("el" (lambda () (interactive) (vesie--emacs-get (vesie--l-dwim 1) "el")))
  ("l" (lambda () (interactive) (vesie--emacs-get (vesie--l-dwim 0) "l")))
  ("d" (lambda () (interactive)
         (beginning-of-thing 'defun) (vesie--emacs-get #'end-of-defun "d")))
  ("w" (lambda () (interactive) (vesie--emacs-get #'forward-word "w")))
  ("s" (lambda () (interactive) (vesie--emacs-get #'forward-sexp "s")))
  (";" (lambda () (interactive) (vesie--emacs-get #'end-of-line ";")))
  ("n" (lambda () (interactive)
         (beginning-of-line) (vesie--emacs-get #'end-of-line "n")))
  ("p" (lambda () (interactive)
         (end-of-line) (vesie--emacs-get #'beginning-of-line "p")))
  ("b" (lambda () (interactive)
         (if current-prefix-arg
             (kill-new (buffer-name))
           (kill-new (or (buffer-file-name)
                         default-directory)))))
  ("c" (lambda () (interactive)
         (beginning-of-line) (vesie--emacs-get #'end-of-line "c")))
  ("k" (lambda () (interactive)
         (beginning-of-line) (vesie--emacs-get #'end-of-line "k")))
  ("m" (lambda () (interactive)
         (beginning-of-line) (vesie--emacs-get #'end-of-line "m")))
  ("t" (lambda () (interactive)
         (vesie--emacs-get
          (lambda () (interactive)
            (let ((p (point)))
              (call-interactively #'avy-goto-char)
              (when (> (point) p)
                (forward-char)))) "t")))
  ("f" (lambda () (interactive)
         (vesie--emacs-get
          (lambda () (interactive)
            (let ((p (point)))
              (call-interactively #'avy-goto-char-in-line)
              (when (> (point) p)
                (forward-char)))) "f")))
  ("r" (lambda () (interactive)
         (call-interactively #'avy-goto-char)
         (vesie--emacs-get
          (lambda () (interactive)
            (let ((p (point)))
              (call-interactively #'avy-goto-char)
              (when (> (point) p)
                (forward-char)))) "q"))))

(dotimes (i 10)
  (define-key vesie-mode-map (number-to-string i) #'digit-argument)
  (define-key vesie-emacs/ckm-map (number-to-string i) #'digit-argument))
(define-key vesie-mode-map "-" #'negative-argument)
(define-key vesie-emacs/ckm-map "-" #'negative-argument)

(advice-add 'org-insert-heading-respect-content :after
            (lambda (&rest _)
              (vesie-mode 0)))
(advice-add 'org-meta-return :after
            (lambda (&rest _)
              (vesie-mode 0)))
(advice-add 'org-open-at-point :before
            (lambda (&rest _)
              (xref-push-marker-stack)
              (vesie-mode 1)))
  ;;; view-file 启动由 vesie-mode 而不是 view-mode
(advice-add 'view-mode :around
            (lambda (_orig-func &rest _)
              (vesie-mode 1)
              (when (memq major-mode '(markdown-mode gfm-mode org-mode))
                (hugomd-preview))))
  ;;; xref-find-definitions
(advice-add 'xref-find-definitions :after
            (lambda (&rest _)
              (vesie-mode 1)))

(provide 'vesie)
;;; vesie.el ends here
