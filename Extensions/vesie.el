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
  :version "28.1")

(defvar vesie-emacs/ckm-map (make-sparse-keymap) "Keymap for ckm commands.")
(defsubst vesie--parse-abracket ()
  "返回 < 和 > 左边的位置."
  (let ((p (point)) (c 0) l r)
    (call-interactively #'beginning-of-line)
    (while (not (eolp))
      (when (= (char-after) ?<)
        (if (>= (point) p)
            (cl-incf c)
          (push (point) l)))
      (when (= (char-after) ?>)
        (if (< (point) p)
            (pop l)
          (if (= c 0)
              (push (point) r)
            (cl-decf c))))
      (forward-char))
    (goto-char p)
    (cons (car l) (last r))))

(defsubst vesie--current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun vesie--list-dwim (N)
  "N=1: el, N=0:l."
  (thunk-let ((state (vesie--current-parse-state))
              (lr (vesie--parse-abracket)))
    (if (or (< 0 (car state))
            (nth 3 state))
        (paredit-backward-up)
      (when (and (car lr) (cadr lr))
        (goto-char (car lr))))
    (forward-char N)
    (lambda () (interactive)
      (if (or (< 0 (car state))
              (nth 3 state))
          (if (= N 1)
              (paredit-forward-up)
            (forward-sexp))
        (when (and (cadr lr) (car lr))
          (goto-char (1+ (cadr lr)))))
      (backward-char N))))

(defun vesie--html-tag (N)
  "N=1: et, N=0:at."
  (require 'sgml-mode)
  (while (or (looking-back "/[a-z]*>[ \t\n]*" (- (line-beginning-position) 15))
             (looking-back "-->[ \t\n]*" (- (line-beginning-position) 15)))
    (sgml-skip-tag-backward 1))
  (while (progn (sgml-skip-tag-backward 1) (nth 3 (syntax-ppss))))
  (and (= N 1) (search-forward ">" (point-at-eol) t))
  (lambda () (interactive)
    (and (= N 1) (sgml-skip-tag-backward 1))
    (sgml-skip-tag-forward 1)
    (and (= N 1)(search-backward "<" (point-at-bol) t))))

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

(defun vesie--eval-sexp-dwim ()
  "如果当前所处位置是 list 或字符串或符号结尾则执行这个 sexp.
若果在字符串内，则假设字符串在 list 内，则执行整个 list;
如果在一个符号内，且这个符号以括号开头则跳出整个 list;
否则执行这个符号."
  (interactive)
  (save-excursion
    (let ((state (vesie--current-parse-state)))
      (cond ((equal last-command 'vesie--eval-sexp-dwim) (end-of-defun))
            ((nth 3 state) (up-list (1+ (nth 0 state)) t))
            ((char-equal (char-before) ?\() (up-list (nth 0 state)))
            (t (backward-sexp 1)
               (if (char-equal (char-before) ?\()
                   (up-list (nth 0 state))
                 (forward-sexp 1))))
      (kmacro/my-eval-last-sexp)
      (setq this-command 'vesie--eval-sexp-dwim))))

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

(zerolee-set-key vesie-mode-map
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
           (let ((name (buffer-name)))
             (kill-buffer)
             (when (< 1 (cl-count (current-buffer)
                                  (mapcar #'window-buffer (window-list))))
               (delete-window))))))
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
  (";" #'vesie--eval-sexp-dwim)
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
  ("aS" (lambda () (interactive)
          (backward-sentence)
          (vesie--emacs-get #'forward-sentence "aS")))
  ("aP" (lambda () (interactive)
          (backward-paragraph)
          (vesie--emacs-get #'forward-paragraph "aP")))
  ("a'" (lambda () (interactive)
          (search-backward "'" (point-at-bol) t 1)
          (vesie--emacs-get (lambda () (interactive)
                              (search-forward "'" (point-at-eol) t 2)) "a'")))
  ("qt" (lambda () (interactive)
          (if (looking-at "[ \t\n?\C-j]*<")
              (vesie--emacs-get #'sgml-skip-tag-forward "qt")
            (if (or (nth 3 (syntax-ppss))
                    (not (eq (char-after) ? )))
                (search-backward " "))
            (vesie--emacs-get (lambda () (interactive)
                                (forward-sexp 2)) "qt"))))
  ("at" (lambda () (interactive) (vesie--emacs-get (vesie--html-tag 0) "at")))
  ("e'" (lambda () (interactive)
          (search-backward "'" (point-at-bol) t 1)
          (forward-char 1)
          (vesie--emacs-get (lambda () (interactive)
                              (search-forward "'" (point-at-eol) t 1)
                              (backward-char 1)) "e'")))
  ("et" (lambda () (interactive) (vesie--emacs-get (vesie--html-tag 1) "et")))
  ("el" (lambda () (interactive) (vesie--emacs-get (vesie--list-dwim 1) "el")))
  ("l" (lambda () (interactive) (vesie--emacs-get (vesie--list-dwim 0) "l")))
  ("L" (lambda () (interactive)
         (vesie--emacs-get (lambda () (interactive)
                             (end-of-thing 'url)) "L")))
  ("aL" (lambda () (interactive)
          (beginning-of-thing 'url)
          (vesie--emacs-get (lambda () (interactive)
                              (end-of-thing 'url)) "L")))
  (" " (lambda () (interactive)
         (vesie--emacs-get
          (lambda () (interactive)
            (if (= -1 (prefix-numeric-value current-prefix-arg))
                (beginning-of-thing 'whitespace)
              (end-of-thing 'whitespace))) " ")))
  ("a " (lambda () (interactive)
          (beginning-of-thing 'whitespace)
          (vesie--emacs-get (lambda () (interactive)
                              (end-of-thing 'whitespace)) "a ")))
  ("e " (lambda () (interactive)
          (beginning-of-thing 'whitespace)
          (vesie--emacs-get (lambda () (interactive)
                              (end-of-thing 'whitespace)
                              (and (char-equal (char-before) ? )
                                   (backward-char))) "e ")))
  ("d" (lambda () (interactive)
         (beginning-of-thing 'defun) (vesie--emacs-get #'end-of-defun "d")))
  ("S" (lambda () (interactive) (vesie--emacs-get #'forward-sentence "S")))
  ("P" (lambda () (interactive) (vesie--emacs-get #'forward-paragraph "P")))
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
