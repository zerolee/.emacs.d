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

(defvar vesie-emacs/ckm-map
  (let ((map (make-sparse-keymap)))
    (define-key map "<" (lambda () (interactive)
                          (let ((current-prefix-arg (point-min)))
                            (end-of-line)
                            (vesie--emacs-get #'goto-char "<"))))
    (define-key map ">" (lambda () (interactive)
                          (let ((current-prefix-arg (point-max)))
                            (beginning-of-line)
                            (vesie--emacs-get #'goto-char ">"))))
    (define-key map "i" (lambda () (interactive)
                          (vesie--emacs-get #'beginning-of-line "i")))
    (define-key map "," (lambda () (interactive)
                          (vesie--emacs-get #'vesie--function-arg-end ",")))
    (define-key map "a," (lambda () (interactive)
                           (goto-char (cdr (nth 1 (vesie--function-arg-info))))
                           (vesie--emacs-get #'vesie--function-arg-end ",")))
    (define-key map "aw"
                (lambda () (interactive)
                  (beginning-of-thing 'word)
                  (vesie--emacs-get (lambda () (interactive)
                                      (forward-word)
                                      (and (char-equal (char-after) ? )
                                           (forward-char))) "aw")))
    (define-key map "ew" (lambda () (interactive)
                           (beginning-of-thing 'word)
                           (vesie--emacs-get #'forward-word "ew")))
    (define-key map "as"
                (lambda () (interactive)
                  (beginning-of-thing 'sexp)
                  (vesie--emacs-get (lambda () (interactive)
                                      (forward-sexp)
                                      (and (char-equal (char-after) ? )
                                           (forward-char))) "as")))
    (define-key map "es" (lambda () (interactive)
                           (beginning-of-thing 'sexp)
                           (vesie--emacs-get #'forward-sexp "es")))
    (define-key map "aS" (lambda () (interactive)
                           (backward-sentence)
                           (vesie--emacs-get #'forward-sentence "aS")))
    (define-key map "aP" (lambda () (interactive)
                           (backward-paragraph)
                           (vesie--emacs-get #'forward-paragraph "aP")))
    (define-key map "a'"
                (lambda () (interactive)
                  (search-backward "'" (point-at-bol) t 1)
                  (vesie--emacs-get (lambda () (interactive)
                                      (search-forward "'" (point-at-eol) t 2)) "a'")))
    (define-key map "qt"
                (lambda () (interactive)
                  (if (looking-at "[ \t\n?\C-j]*<")
                      (vesie--emacs-get #'sgml-skip-tag-forward "qt")
                    (if (or (nth 3 (syntax-ppss))
                            (not (eq (char-after) ? )))
                        (search-backward " "))
                    (vesie--emacs-get (lambda () (interactive)
                                        (forward-sexp 2)) "qt"))))
    (define-key map "at" (lambda () (interactive)
                           (vesie--emacs-get (vesie--html-tag 0) "at")))
    (define-key map "e'"
                (lambda () (interactive)
                  (search-backward "'" (point-at-bol) t 1)
                  (forward-char 1)
                  (vesie--emacs-get (lambda () (interactive)
                                      (search-forward "'" (point-at-eol) t 1)
                                      (backward-char 1)) "e'")))
    (define-key map "et" (lambda () (interactive)
                           (vesie--emacs-get (vesie--html-tag 1) "et")))
    (define-key map "el" (lambda () (interactive)
                           (vesie--emacs-get (vesie--list-dwim 1) "el")))
    (define-key map "l" (lambda () (interactive)
                          (vesie--emacs-get (vesie--list-dwim 0) "l")))
    (define-key map "L" (lambda () (interactive)
                          (vesie--emacs-get (lambda () (interactive)
                                              (end-of-thing 'url)) "L")))
    (define-key map "aL" (lambda () (interactive)
                           (beginning-of-thing 'url)
                           (vesie--emacs-get (lambda () (interactive)
                                               (end-of-thing 'url)) "L")))
    (define-key map " "
                (lambda () (interactive)
                  (vesie--emacs-get
                   (lambda () (interactive)
                     (if (= -1 (prefix-numeric-value current-prefix-arg))
                         (beginning-of-thing 'whitespace)
                       (end-of-thing 'whitespace))) " ")))
    (define-key map "a "
                (lambda () (interactive)
                  (beginning-of-thing 'whitespace)
                  (vesie--emacs-get (lambda () (interactive)
                                      (end-of-thing 'whitespace)) "a ")))
    (define-key map "e "
                (lambda () (interactive)
                  (beginning-of-thing 'whitespace)
                  (vesie--emacs-get (lambda () (interactive)
                                      (end-of-thing 'whitespace)
                                      (and (char-equal (char-before) ? )
                                           (backward-char))) "e ")))
    (define-key map "d" (lambda () (interactive)
                          (beginning-of-thing 'defun)
                          (vesie--emacs-get #'end-of-defun "d")))
    (define-key map "S" (lambda () (interactive)
                          (vesie--emacs-get #'forward-sentence "S")))
    (define-key map "P" (lambda () (interactive)
                          (vesie--emacs-get #'forward-paragraph "P")))
    (define-key map "w" (lambda () (interactive)
                          (vesie--emacs-get #'forward-word "w")))
    (define-key map "s" (lambda () (interactive)
                          (vesie--emacs-get #'forward-sexp "s")))
    (define-key map ";" (lambda () (interactive)
                          (vesie--emacs-get #'end-of-line ";")))
    (define-key map "n" (lambda () (interactive)
                          (beginning-of-line)
                          (vesie--emacs-get #'end-of-line "n")))
    (define-key map "p" (lambda () (interactive)
                          (end-of-line)
                          (vesie--emacs-get #'beginning-of-line "p")))
    (define-key map "b" (lambda () (interactive)
                          (if current-prefix-arg
                              (kill-new (buffer-name))
                            (kill-new (or (buffer-file-name)
                                          default-directory)))))
    (define-key map "c" (lambda () (interactive)
                          (beginning-of-line)
                          (vesie--emacs-get #'end-of-line "c")))
    (define-key map "k" (lambda () (interactive)
                          (beginning-of-line)
                          (vesie--emacs-get #'end-of-line "k")))
    (define-key map "m" (lambda () (interactive)
                          (beginning-of-line)
                          (vesie--emacs-get #'end-of-line "m")))
    (define-key map "t" (lambda () (interactive)
                          (vesie--emacs-get
                           (lambda () (interactive)
                             (let ((p (point)))
                               (call-interactively #'avy-goto-char)
                               (when (> (point) p)
                                 (forward-char)))) "t")))
    (define-key map "f" (lambda () (interactive)
                          (vesie--emacs-get
                           (lambda () (interactive)
                             (let ((p (point)))
                               (call-interactively #'avy-goto-char-in-line)
                               (when (> (point) p)
                                 (forward-char)))) "f")))
    (define-key map "r" (lambda () (interactive)
                          (call-interactively #'avy-goto-char)
                          (vesie--emacs-get
                           (lambda () (interactive)
                             (let ((p (point)))
                               (call-interactively #'avy-goto-char)
                               (when (> (point) p)
                                 (forward-char)))) "q")))
    map)
  "Keymap for ckm commands.")
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
      (my-eval-last-sexp)
      (setq this-command 'vesie--eval-sexp-dwim))))

(defun vesie--function-arg-pinfo ()
  "获取当前函数所有参数的位置信息，两个一组以列表的方式返回."
  (save-excursion
    (let (position
          positions)
      (goto-char (1+ (nth 1 (syntax-ppss))))
      (setq position (point-marker))
      (while (not (char-equal (char-after) ?\)))
        (forward-sexp)
        (when (char-equal (char-after) ?\,)
          (push (point-marker) position)
          (push position positions)
          (forward-char)
          (setq position (point-marker))))
      (push (point-marker) position)
      (push position positions)
      (reverse positions))))

(defun vesie--function-arg-info ()
  "通过 PINFO 获取当前位置当前参数的位置信息.
return: (first current last), 每个元素由 (right . left)组成， 不存在则为 nil."
  (let ((cp (point))
        (pinfo (vesie--function-arg-pinfo))
        first)
    (catch 'done
      (while pinfo
        (let ((current (car pinfo)))
          (if (<= cp (marker-position (car current)))
              (throw 'done (list first current (cadr pinfo)))
            (setq first current)
            (setq pinfo (cdr pinfo))))))))

(defun vesie--function-arg-end ()
  "去当前位置的终点."
  (interactive)
  (goto-char (car (nth 1 (vesie--function-arg-info)))))

(defun vesie--function-arg-goto (index &optional overlay)
  "去指定位置 INDEX，并返回该位置坐标, OVERLAY 存在则移动 OVERLAY."
  (let ((current (nth index (vesie--function-arg-info))))
    (when current
      (goto-char (cdr current))
      (when overlay
        (move-overlay overlay (cdr current) (car current)))
      (while (member (char-after) '(?\C-j ? ?\C-i))
        (forward-char 1))
      current)))

(let (overlay
      current
      (map (make-sparse-keymap)))
  (define-key map (kbd "<tab>") #'vesie--function-arg-next)
  (define-key map (kbd "<backtab>") #'vesie--function-arg-prev)
  (defun vesie--function-arg-overlay ()
    (setq current (nth 1 (vesie--function-arg-info)))
    (setq overlay (make-overlay (cdr current) (car current)))
    (overlay-put overlay 'face 'highlight)
    (overlay-put overlay 'keymap map))
  (defun vesie--function-arg-next ()
    (interactive)
    (unless (vesie--function-arg-goto 2 overlay)
      (up-list 1 t)
      (delete-overlay overlay)
      (while (member (char-after) '(?\, ?\) ?\;))
        (forward-char))))
  (defun vesie--function-arg-prev ()
    (interactive)
    (vesie--function-arg-goto 0 overlay)))

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

(define-key vesie-mode-map (kbd "a") #'beginning-of-line)
(define-key vesie-mode-map (kbd "A") (lambda () (interactive)
                                       (beginning-of-line)
                                       (forward-to-word 1)))
(define-key vesie-mode-map (kbd "C-a") (lambda () (interactive)
                                         (beginning-of-line)
                                         (vesie-mode 0)))
(define-key vesie-mode-map (kbd "b") #'backward-char)
(define-key vesie-mode-map (kbd "B") (lambda () (interactive)
                                       (vesie--function-arg-goto 0)))
(define-key vesie-mode-map (kbd "C-b") (lambda () (interactive)
                                         (vesie-mode 0)))
(define-key vesie-mode-map (kbd "c") (lambda () (interactive)
                                       (vesie-ckm "c")
                                       (vesie-mode 0)))
(define-key vesie-mode-map (kbd "d") #'delete-char)
(define-key vesie-mode-map (kbd "C-d") (lambda () (interactive)
                                         (delete-char 1)
                                         (vesie-mode 0)))
(define-key vesie-mode-map (kbd "e") #'move-end-of-line)
(define-key vesie-mode-map (kbd "C-e") (lambda () (interactive)
                                         (move-end-of-line 1)
                                         (vesie-mode 0)))
(define-key vesie-mode-map (kbd "f") #'forward-char)
(define-key vesie-mode-map (kbd "F") (lambda () (interactive)
                                       (vesie--function-arg-goto 2)))
(define-key vesie-mode-map (kbd "C-f") (lambda () (interactive)
                                         (or (eolp)
                                             (forward-char))
                                         (vesie-mode 0)))
(define-key vesie-mode-map (kbd "g") #'goto-line)
(define-key vesie-mode-map (kbd "h") #'paredit-backward)
(define-key vesie-mode-map (kbd "H") (lambda () (interactive)
                                       (save-excursion
                                         (delete-indentation))))
(define-key vesie-mode-map (kbd "i") #'vesie-mode)
(define-key vesie-mode-map (kbd "I") #'beginning-of-line-text)
(define-key vesie-mode-map (kbd "j")
            (lambda () (interactive)
              (if (minibufferp (current-buffer))
                  (call-interactively #'ivy-done)
                (call-interactively #'forward-to-indentation))))
(define-key vesie-mode-map (kbd "C-j") (lambda () (interactive)
                                         (newline-and-indent)
                                         (vesie-mode 0)))
(define-key vesie-mode-map (kbd "k") (lambda () (interactive)
                                       (vesie-ckm "k")))
(define-key vesie-mode-map (kbd "l") #'paredit-forward)
(define-key vesie-mode-map (kbd "L") (lambda () (interactive)
                                       (save-excursion
                                         (delete-indentation 1))))
(define-key vesie-mode-map (kbd "m") (lambda () (interactive)
                                       (vesie-ckm "m")))
(define-key vesie-mode-map (kbd "M") (lambda () (interactive)
                                       (save-excursion
                                         (call-interactively #'mark-whole-buffer)
                                         (call-interactively #'indent-for-tab-command)
                                         (whitespace-cleanup)
                                         (call-interactively #'untabify))))
(define-key vesie-mode-map (kbd "n")
            (lambda () (interactive)
              (if (equal major-mode 'xref--xref-buffer-mode)
                  (xref-next-line)
                (if (minibufferp (current-buffer))
                    (call-interactively #'ivy-next-line)
                  (call-interactively #'next-line)))))
(define-key vesie-mode-map (kbd "C-n") (lambda () (interactive)
                                         (call-interactively #'next-line)
                                         (vesie-mode 0)))
(define-key vesie-mode-map (kbd "N") (lambda () (interactive)
                                       (save-excursion
                                         (end-of-line)
                                         (open-line 1))))
(define-key vesie-mode-map (kbd "M-n") (lambda () (interactive)
                                         (save-excursion
                                           (end-of-line)
                                           (newline-and-indent)
                                           (yank))))
(define-key vesie-mode-map (kbd "o") (lambda () (interactive)
                                       (end-of-line)
                                       (newline-and-indent)
                                       (vesie-mode 0)))
(define-key vesie-mode-map (kbd "O") (lambda () (interactive)
                                       (call-interactively #'vesie-mode)
                                       (beginning-of-line)
                                       (open-line 1)
                                       (call-interactively #'indent-for-tab-command)))
(define-key vesie-mode-map (kbd "p")
            (lambda () (interactive)
              (if (eq major-mode 'xref--xref-buffer-mode)
                  (xref-prev-line)
                (if (minibufferp (current-buffer))
                    (call-interactively #'ivy-previous-line)
                  (call-interactively #'previous-line)))))
(define-key vesie-mode-map (kbd "P")
            (lambda () (interactive)
              (save-excursion
                (beginning-of-line)
                (open-line 1))
              (when (bolp)
                (call-interactively #'next-line))))
(define-key vesie-mode-map (kbd "M-p")
            (lambda () (interactive)
              (save-excursion
                (beginning-of-line)
                (open-line 1)
                (call-interactively #'indent-for-tab-command)
                (yank))))
(define-key vesie-mode-map (kbd "q")
            (lambda () (interactive)
              (if (minibufferp (current-buffer))
                  (call-interactively #'minibuffer-keyboard-quit)
                (let ((name (buffer-name)))
                  (kill-buffer)
                  (if (< 1 (cl-count (current-buffer)
                                     (mapcar #'window-buffer (window-list))))
                      (delete-window)
                    (when (member name '("*lsp-help*"))
                      (other-window -1)))))))
(define-key vesie-mode-map (kbd "Q") #'kill-buffer-and-window)
(define-key vesie-mode-map (kbd "r") (lambda (char) (interactive "*c")
                                       (delete-char 1)
                                       (insert-char char)))
(define-key vesie-mode-map (kbd "R") (lambda () (interactive)
                                       (vesie-mode 0)
                                       (setq cursor-type 'box)
                                       (overwrite-mode 1)))
(define-key vesie-mode-map (kbd "s") #'avy-goto-char-in-line)
(define-key vesie-mode-map (kbd "t") #'avy-goto-char)
(define-key vesie-mode-map (kbd "u") #'undo)
(define-key vesie-mode-map (kbd "v") #'scroll-up-command)
(define-key vesie-mode-map (kbd "w") #'forward-to-word)
(define-key vesie-mode-map (kbd "W")
            (lambda () (interactive)
              (vesie--function-arg-overlay)
              (vesie-mode 0)))
(define-key vesie-mode-map (kbd "x")
            (lambda () (interactive)
              (hs-minor-mode)
              (hs-toggle-hiding)))
(define-key vesie-mode-map (kbd "X")
            (lambda () (interactive)
              (if (bound-and-true-p hs-minor-mode)
                  (hs-minor-mode -1)
                (hs-minor-mode)
                (hs-hide-all))))
(define-key vesie-mode-map (kbd "y") #'yank)
(define-key vesie-mode-map (kbd "z") #'save-buffer)
(define-key vesie-mode-map (kbd "Z") #'save-buffers-kill-terminal)
(define-key vesie-mode-map (kbd "(") #'paredit-backward-slurp-sexp)
(define-key vesie-mode-map (kbd ")") #'paredit-forward-slurp-sexp)
(define-key vesie-mode-map (kbd "[") #'paredit-add-to-previous-list)
(define-key vesie-mode-map (kbd "]") #'paredit-add-to-next-list)
(define-key vesie-mode-map (kbd "<") #'paredit-backward-barf-sexp)
(define-key vesie-mode-map (kbd ">") #'paredit-forward-barf-sexp)
(define-key vesie-mode-map (kbd "S") #'paredit-split-sexp)
(define-key vesie-mode-map (kbd "J") #'paredit-join-sexps)
(define-key vesie-mode-map (kbd "<up>") (lambda () (interactive)
                                          (paredit-backward)
                                          (paredit-raise-sexp)))
(define-key vesie-mode-map (kbd "<down>") #'paredit-raise-sexp)
(define-key vesie-mode-map (kbd "<left>") #'paredit-splice-sexp-killing-forward)
(define-key vesie-mode-map (kbd "<right>") #'paredit-splice-sexp-killing-backward)
(define-key vesie-mode-map (kbd ";") #'vesie--eval-sexp-dwim)
(define-key vesie-mode-map (kbd ".") #'repeat)

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
