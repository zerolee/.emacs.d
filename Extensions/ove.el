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
(require 'thunk)

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
  :version "28.1")

(defsubst ove--parse-abracket ()
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

(defsubst ove--current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun ove--list-dwim (N)
  "N=1: el, N=0:l."
  (thunk-let ((state (ove--current-parse-state))
              (lr (ove--parse-abracket)))
    (if (or (< 0 (car state))
            (nth 3 state))
        (paredit-backward-up)
      (when (and (car lr) (cadr lr))
        (goto-char (car lr))))
    (forward-char N)
    #'(lambda () (interactive)
        (if (or (< 0 (car state))
                (nth 3 state))
            (if (= N 1)
                (paredit-forward-up)
              (forward-sexp))
          (when (and (cadr lr) (car lr))
            (goto-char (1+ (cadr lr)))))
        (backward-char N))))

(defun ove--html-tag (N)
  "N=1: et, N=0:at."
  (require 'sgml-mode)
  (while (thing-at-point-looking-at "</[a-z]+>[ \n]*")
    (sgml-skip-tag-backward 1))
  (sgml-skip-tag-backward 1)
  (if (string= "<!--" (buffer-substring-no-properties (point) (+ (point) 4)))
      (sgml-skip-tag-backward 1))
  (and (= N 1) (search-forward ">" (point-at-eol) t))
  #'(lambda () (interactive)
      (and (= N 1) (sgml-skip-tag-backward 1))
      (sgml-skip-tag-forward 1)
      (and (= N 1)(search-backward "<" (point-at-bol) t))))

(defvar ove-emacs/ckm-map
  (let ((map (make-sparse-keymap)))
    (define-key map "<" #'(lambda () (interactive)
                            (let ((current-prefix-arg (point-min)))
                              (end-of-line)
                              (ove--emacs-get #'goto-char "<"))))
    (define-key map ">" #'(lambda () (interactive)
                            (let ((current-prefix-arg (point-max)))
                              (beginning-of-line)
                              (ove--emacs-get #'goto-char ">"))))
    (define-key map "i" #'(lambda () (interactive)
                            (ove--emacs-get #'beginning-of-line "i")))
    (define-key map "," #'(lambda () (interactive)
                            (ove--emacs-get #'ove--function-arg-end ",")))
    (define-key map "a," #'(lambda () (interactive)
                             (goto-char (cdr (nth 1 (ove--function-arg-info))))
                             (ove--emacs-get #'ove--function-arg-end ",")))
    (define-key map "aw"
      #'(lambda () (interactive)
          (beginning-of-thing 'word)
          (ove--emacs-get #'(lambda () (interactive)
                              (forward-word)
                              (and (char-equal (char-after) ? )
                                   (forward-char))) "aw")))
    (define-key map "ew" #'(lambda () (interactive)
                             (beginning-of-thing 'word)
                             (ove--emacs-get #'forward-word "ew")))
    (define-key map "as"
      #'(lambda () (interactive)
          (beginning-of-thing 'sexp)
          (ove--emacs-get #'(lambda () (interactive)
                              (forward-sexp)
                              (and (char-equal (char-after) ? )
                                   (forward-char))) "as")))
    (define-key map "es" #'(lambda () (interactive)
                             (beginning-of-thing 'sexp)
                             (ove--emacs-get #'forward-sexp "es")))
    (define-key map "aS" #'(lambda () (interactive)
                             (backward-sentence)
                             (ove--emacs-get #'forward-sentence "aS")))
    (define-key map "aP" #'(lambda () (interactive)
                             (backward-paragraph)
                             (ove--emacs-get #'forward-paragraph "aP")))
    (define-key map "a'"
      #'(lambda () (interactive)
          (search-backward "'" (point-at-bol) t 1)
          (ove--emacs-get '(lambda () (interactive)
                             (search-forward "'" (point-at-eol) t 2)) "a'")))
    (define-key map "at" #'(lambda () (interactive)
                             (ove--emacs-get (ove--html-tag 0) "at")))
    (define-key map "e'"
      #'(lambda () (interactive)
          (search-backward "'" (point-at-bol) t 1)
          (forward-char 1)
          (ove--emacs-get '(lambda () (interactive)
                             (search-forward "'" (point-at-eol) t 1)
                             (backward-char 1)) "e'")))
    (define-key map "et" #'(lambda () (interactive)
                             (ove--emacs-get (ove--html-tag 1) "et")))
    (define-key map "el" #'(lambda () (interactive)
                             (ove--emacs-get (ove--list-dwim 1) "el")))
    (define-key map "l" #'(lambda () (interactive)
                            (ove--emacs-get (ove--list-dwim 0) "l")))
    (define-key map "L" #'(lambda () (interactive)
                            (ove--emacs-get #'(lambda () (interactive)
                                                (end-of-thing 'url)) "L")))
    (define-key map "aL" #'(lambda () (interactive)
                             (beginning-of-thing 'url)
                             (ove--emacs-get #'(lambda () (interactive)
                                                 (end-of-thing 'url)) "L")))
    (define-key map " "
      #'(lambda () (interactive)
          (ove--emacs-get
           #'(lambda () (interactive)
               (if (= -1 (prefix-numeric-value current-prefix-arg))
                   (beginning-of-thing 'whitespace)
                 (end-of-thing 'whitespace))) " ")))
    (define-key map "a "
      #'(lambda () (interactive)
          (beginning-of-thing 'whitespace)
          (ove--emacs-get #'(lambda () (interactive)
                              (end-of-thing 'whitespace)) "a ")))
    (define-key map "e "
      #'(lambda () (interactive)
          (beginning-of-thing 'whitespace)
          (ove--emacs-get #'(lambda () (interactive)
                              (end-of-thing 'whitespace)
                              (and (char-equal (char-before) ? )
                                   (backward-char))) "e ")))
    (define-key map "d" #'(lambda () (interactive)
                            (beginning-of-thing 'defun)
                            (ove--emacs-get #'end-of-defun "d")))
    (define-key map "S" #'(lambda () (interactive)
                            (ove--emacs-get #'forward-sentence "S")))
    (define-key map "P" #'(lambda () (interactive)
                            (ove--emacs-get #'forward-paragraph "P")))
    (define-key map "w" #'(lambda () (interactive)
                            (ove--emacs-get #'forward-word "w")))
    (define-key map "s" #'(lambda () (interactive)
                            (ove--emacs-get #'forward-sexp "s")))
    (define-key map ";" #'(lambda () (interactive)
                            (ove--emacs-get #'end-of-line ";")))
    (define-key map "n" #'(lambda () (interactive)
                            (beginning-of-line)
                            (ove--emacs-get #'end-of-line "n")))
    (define-key map "p" #'(lambda () (interactive)
                            (end-of-line)
                            (ove--emacs-get #'beginning-of-line "p")))
    (define-key map "b" #'(lambda () (interactive)
                            (if current-prefix-arg
                                (kill-new (buffer-name))
                              (kill-new (or (buffer-file-name)
                                            default-directory)))))
    (define-key map "c" #'(lambda () (interactive)
                            (beginning-of-line)
                            (ove--emacs-get #'end-of-line "c")))
    (define-key map "k" #'(lambda () (interactive)
                            (beginning-of-line)
                            (ove--emacs-get #'end-of-line "k")))
    (define-key map "m" #'(lambda () (interactive)
                            (beginning-of-line)
                            (ove--emacs-get #'end-of-line "m")))
    (define-key map "t" #'(lambda () (interactive)
                            (ove--emacs-get
                             #'(lambda () (interactive)
                                 (let ((p (point)))
                                   (call-interactively #'avy-goto-char)
                                   (when (> (point) p)
                                     (forward-char)))) "t")))
    (define-key map "f" #'(lambda () (interactive)
                            (ove--emacs-get
                             #'(lambda () (interactive)
                                 (let ((p (point)))
                                   (call-interactively #'avy-goto-char-in-line)
                                   (when (> (point) p)
                                     (forward-char)))) "f")))
    map)
  "Keymap for ckm commands.")
(let (ove-arg1
      ove-kill-or-save
      emacs-ckm-point
      cpa)
  (defun ove-ckm (which-ckm)
    (setq ove-arg1 which-ckm
          emacs-ckm-point (point)
          cpa current-prefix-arg)
    (if (string-equal which-ckm "m")
        (setq ove-kill-or-save #'kill-ring-save)
      (setq ove-kill-or-save #'kill-region))
    (set-transient-map ove-emacs/ckm-map t))

  (defun ove--emacs-get (ove-move ove-arg2)
    "删除或者保存 region 中的数据."
    (setq current-prefix-arg (prefix-numeric-value (or cpa current-prefix-arg)))
    (when (string= ove-arg2 "p")
      (setq current-prefix-arg (- 1 current-prefix-arg)))
    (when (string= ove-arg2 "n")
      (cl-incf current-prefix-arg))
    (let ((current-position (point)))
      (funcall ove-kill-or-save current-position
               (progn
                 (call-interactively ove-move)
                 (pulse-momentary-highlight-region
                  current-position (point) 'ove-aquamarine)
                 (point))))
    ;; k 与 c 的区别
    (and (string= ove-arg1 "k") (string-match ove-arg2 "<>npk")
         (char-after) (delete-char 1))
    ;; 如果复制的话，恢复其位置
    (when (string-equal ove-arg1 "m")
      (goto-char emacs-ckm-point))
    (setq overriding-terminal-local-map nil
          cpa nil)))

(defun ove--eval-sexp-dwim ()
  "如果当前所处位置是 list 或字符串或符号结尾则执行这个 sexp.
若果在字符串内，则假设字符串在 list 内，则执行整个 list;
如果在一个符号内，且这个符号以括号开头则跳出整个 list;
否则执行这个符号."
  (interactive)
  (save-excursion
    (let ((state (ove--current-parse-state)))
      (cond ((equal last-command 'ove--eval-sexp-dwim) (end-of-defun))
            ((nth 3 state) (up-list (1+ (nth 0 state)) t))
            ((char-equal (char-before) ?\() (up-list (nth 0 state)))
            (t (backward-sexp 1)
               (if (char-equal (char-before) ?\()
                   (up-list (nth 0 state))
                 (forward-sexp 1))))
      (my-eval-last-sexp)
      (setq this-command 'ove--eval-sexp-dwim))))

(defun ove--function-arg-pinfo ()
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

(defun ove--function-arg-info ()
  "通过 PINFO 获取当前位置当前参数的位置信息.
return: (first current last), 每个元素由 (right . left)组成， 不存在则为 nil."
  (let ((cp (point))
        (pinfo (ove--function-arg-pinfo))
        first)
    (catch 'done
      (while pinfo
        (let ((current (car pinfo)))
          (if (<= cp (marker-position (car current)))
              (throw 'done (list first current (cadr pinfo)))
            (setq first current)
            (setq pinfo (cdr pinfo))))))))

(defun ove--function-arg-end ()
  "去当前位置的终点."
  (interactive)
  (goto-char (car (nth 1 (ove--function-arg-info)))))

(defun ove--function-arg-goto (index &optional overlay)
  "去指定位置 INDEX，并返回该位置坐标, OVERLAY 存在则移动 OVERLAY."
  (let ((current (nth index (ove--function-arg-info))))
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
  (define-key map (kbd "<tab>") #'ove--function-arg-next)
  (define-key map (kbd "<backtab>") #'ove--function-arg-prev)
  (defun ove--function-arg-overlay ()
    (setq current (nth 1 (ove--function-arg-info)))
    (setq overlay (make-overlay (cdr current) (car current)))
    (overlay-put overlay 'face 'highlight)
    (overlay-put overlay 'keymap map))
  (defun ove--function-arg-next ()
    (interactive)
    (unless (ove--function-arg-goto 2 overlay)
      (up-list 1 t)
      (delete-overlay overlay)
      (while (member (char-after) '(?\, ?\) ?\;))
        (forward-char))))
  (defun ove--function-arg-prev ()
    (interactive)
    (ove--function-arg-goto 0 overlay)))

;;;###autoload
(define-minor-mode ove-mode
  "拥有 vim 模式的 Emacs 风格的 minor"
  :init-value nil
  :lighter ""
  :keymap (make-sparse-keymap)
  (if (not ove-mode) (setq cursor-type 'bar)
    (setq cursor-type 'box)
    (call-process "fcitx-remote" nil nil nil "-c"))
  (overwrite-mode 0))

(define-key ove-mode-map (kbd "a") #'beginning-of-line)
(define-key ove-mode-map (kbd "A") #'(lambda () (interactive)
                                       (beginning-of-line)
                                       (forward-to-word 1)))
(define-key ove-mode-map (kbd "C-a") #'(lambda () (interactive)
                                         (beginning-of-line)
                                         (ove-mode 0)))
(define-key ove-mode-map (kbd "b") #'backward-char)
(define-key ove-mode-map (kbd "B") #'(lambda () (interactive)
                                       (ove--function-arg-goto 0)))
(define-key ove-mode-map (kbd "C-b") #'(lambda () (interactive)
                                         (ove-mode 0)))
(define-key ove-mode-map (kbd "c") #'(lambda () (interactive)
                                       (ove-ckm "c")
                                       (ove-mode 0)))
(define-key ove-mode-map (kbd "d") #'delete-char)
(define-key ove-mode-map (kbd "C-d") '(lambda () (interactive)
                                        (delete-char 1)
                                        (ove-mode 0)))
(define-key ove-mode-map (kbd "e") #'move-end-of-line)
(define-key ove-mode-map (kbd "C-e") #'(lambda () (interactive)
                                         (move-end-of-line 1)
                                         (ove-mode 0)))
(define-key ove-mode-map (kbd "f") #'forward-char)
(define-key ove-mode-map (kbd "F") #'(lambda () (interactive)
                                       (ove--function-arg-goto 2)))
(define-key ove-mode-map (kbd "C-f") #'(lambda () (interactive)
                                         (or (eolp)
                                             (forward-char))
                                         (ove-mode 0)))
(define-key ove-mode-map (kbd "g") #'goto-line)
(define-key ove-mode-map (kbd "h") #'paredit-backward)
(define-key ove-mode-map (kbd "H") #'(lambda () (interactive)
                                       (save-excursion
                                         (delete-indentation))))
(define-key ove-mode-map (kbd "i") #'ove-mode)
(define-key ove-mode-map (kbd "I") #'beginning-of-line-text)
(define-key ove-mode-map (kbd "j") #'forward-to-indentation)
(define-key ove-mode-map (kbd "C-j") #'(lambda () (interactive)
                                         (newline-and-indent)
                                         (ove-mode 0)))
(define-key ove-mode-map (kbd "k") #'(lambda () (interactive)
                                       (ove-ckm "k")))
(define-key ove-mode-map (kbd "l") #'paredit-forward)
(define-key ove-mode-map (kbd "L") #'(lambda () (interactive)
                                       (save-excursion
                                         (delete-indentation 1))))
(define-key ove-mode-map (kbd "m") #'(lambda () (interactive)
                                       (ove-ckm "m")))
(define-key ove-mode-map (kbd "M") #'(lambda () (interactive)
                                       (save-excursion
                                         (call-interactively #'mark-whole-buffer)
                                         (mytab)
                                         (whitespace-cleanup)
                                         (call-interactively #'untabify))))
(define-key ove-mode-map (kbd "n")
  #'(lambda () (interactive)
      (if (equal major-mode 'xref--xref-buffer-mode)
          (xref-next-line)
        (call-interactively #'next-line))))
(define-key ove-mode-map (kbd "C-n") #'(lambda () (interactive)
                                         (call-interactively #'next-line)
                                         (ove-mode 0)))
(define-key ove-mode-map (kbd "N") #'(lambda () (interactive)
                                       (save-excursion
                                         (end-of-line)
                                         (open-line 1))))
(define-key ove-mode-map (kbd "M-n") #'(lambda () (interactive)
                                         (end-of-line)
                                         (newline-and-indent)
                                         (yank)))
(define-key ove-mode-map (kbd "o") #'(lambda () (interactive)
                                       (end-of-line)
                                       (newline-and-indent)
                                       (ove-mode 0)))
(define-key ove-mode-map (kbd "O") #'(lambda () (interactive)
                                       (call-interactively #'ove-mode)
                                       (beginning-of-line)
                                       (open-line 1)
                                       (mytab)))
(define-key ove-mode-map (kbd "p")
  #'(lambda () (interactive)
      (if (eq major-mode 'xref--xref-buffer-mode)
          (xref-prev-line)
        (call-interactively #'previous-line))))
(define-key ove-mode-map (kbd "P")
  #'(lambda () (interactive)
      (save-excursion
        (beginning-of-line)
        (open-line 1))
      (when (bolp)
        (call-interactively #'next-line))))
(define-key ove-mode-map (kbd "M-p")
  #'(lambda () (interactive)
      (beginning-of-line)
      (open-line 1)
      (mytab)
      (yank)))
(define-key ove-mode-map (kbd "q")
  #'(lambda () (interactive)
      (let ((name (buffer-name)))
        (kill-buffer)
        (if (< 1 (cl-count (current-buffer)
                           (mapcar #'window-buffer (window-list))))
            (delete-window)
          (when (member name '("*lsp-help*"))
            (other-window -1))))))
(define-key ove-mode-map (kbd "Q") #'kill-buffer-and-window)
(define-key ove-mode-map (kbd "r") #'(lambda (char) (interactive "*c")
                                       (delete-char 1)
                                       (insert-char char)))
(define-key ove-mode-map (kbd "R") #'(lambda () (interactive)
                                       (ove-mode 0)
                                       (setq cursor-type 'box)
                                       (overwrite-mode 1)))
;; (define-key ove-mode-map (kbd "s") 'backward-char)
;; (define-key ove-mode-map (kbd "t") 'backward-char)
(define-key ove-mode-map (kbd "u") #'undo)
(define-key ove-mode-map (kbd "v") #'scroll-up-command)
(define-key ove-mode-map (kbd "w") #'forward-to-word)
(define-key ove-mode-map (kbd "W")
  #'(lambda () (interactive)
      (ove--function-arg-overlay)
      (ove-mode 0)))
(define-key ove-mode-map (kbd "x")
  #'(lambda () (interactive)
      (hs-minor-mode)
      (hs-toggle-hiding)))
(define-key ove-mode-map (kbd "X")
  #'(lambda () (interactive)
      (if (bound-and-true-p hs-minor-mode)
          (hs-minor-mode -1)
        (hs-minor-mode)
        (hs-hide-all))))
(define-key ove-mode-map (kbd "y") #'yank)
(define-key ove-mode-map (kbd "z") #'save-buffer)
(define-key ove-mode-map (kbd "Z") #'save-buffers-kill-terminal)
(define-key ove-mode-map (kbd "(") #'paredit-backward-slurp-sexp)
(define-key ove-mode-map (kbd ")") #'paredit-forward-slurp-sexp)
(define-key ove-mode-map (kbd "[") #'paredit-add-to-previous-list)
(define-key ove-mode-map (kbd "]") #'paredit-add-to-next-list)
(define-key ove-mode-map (kbd "<") #'paredit-backward-barf-sexp)
(define-key ove-mode-map (kbd ">") #'paredit-forward-barf-sexp)
(define-key ove-mode-map (kbd "S") #'paredit-split-sexp)
(define-key ove-mode-map (kbd "J") #'paredit-join-sexps)
(define-key ove-mode-map (kbd "<up>") #'(lambda () (interactive)
                                          (paredit-backward)
                                          (paredit-raise-sexp)))
(define-key ove-mode-map (kbd "<down>") #'paredit-raise-sexp)
(define-key ove-mode-map (kbd "<left>") #'paredit-splice-sexp-killing-forward)
(define-key ove-mode-map (kbd "<right>") #'paredit-splice-sexp-killing-backward)
(define-key ove-mode-map (kbd ";") #'ove--eval-sexp-dwim)
(define-key ove-mode-map (kbd ".") #'repeat)

(dotimes (i 10)
  (define-key ove-mode-map (number-to-string i) #'digit-argument)
  (define-key ove-emacs/ckm-map (number-to-string i) #'digit-argument))
(define-key ove-mode-map "-" #'negative-argument)
(define-key ove-emacs/ckm-map "-" #'negative-argument)

(advice-add 'org-insert-heading-respect-content :after
            #'(lambda (&rest _)
                (ove-mode 0)))
(advice-add 'org-meta-return :after
            #'(lambda (&rest _)
                (ove-mode 0)))
(advice-add 'org-open-at-point :before
            #'(lambda (&rest _)
                (xref-push-marker-stack)
                (ove-mode 1)))
  ;;; view-file 启动由 ove-mode 而不是 view-mode
(advice-add 'view-mode :around
            #'(lambda (_orig-func &rest _)
                (ove-mode 1)
                (when (memq major-mode '(markdown-mode gfm-mode org-mode))
                  (hugomd-preview))))
  ;;; xref-find-definitions
(advice-add 'xref-find-definitions :after
            #'(lambda (&rest _)
                (ove-mode 1)))

(provide 'ove)
;;; ove.el ends here
