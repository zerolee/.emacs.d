;; save-position
(autoload 'sp-push-position-to-ring "save-position")

;;; 第一次安装时去掉注释
;;;(progn
;;;  (package-refresh-contents)
;;;  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package markdown-mode)
(use-package hydra)
(use-package projectile)

(use-package goto-chg
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package multiple-cursors-core
  :ensure multiple-cursors
  :bind (("M-g m" . mc/edit-lines)
         ("M-g r" . mc/mark-all-in-region-regexp)
         ("M-g a" . mc/mark-all-like-this)
         ("M-g W" . mc/mark-next-like-this-word)
         ("M-g S" . mc/mark-next-like-this-symbol)
         :map mc/keymap
         ("M-p" . mc/mark-previous-like-this)
         ("M-P" . mc/unmark-previous-like-this)
         ("M-n" . mc/mark-next-like-this)
         ("M-N" . mc/unmark-next-like-this)
         ("M-s" . mc/skip-to-next-like-this)
         ("M-S" . mc/skip-to-previous-like-this)
         ("M-i" . mc/insert-numbers)))

(use-package treemacs
  :bind (("<f2>" . treemacs)
         :map treemacs-mode-map
         ("m" . (lambda () (interactive)
                  (let ((bname (buffer-name)))
                    (treemacs-RET-action)
                    (or (string-equal bname (buffer-name)) (other-window -1)))))))

(use-package expand-region
  :bind ("C-=" . er/expand-region))


(use-package iedit
  :bind ("M-i" . iedit-mode))

(use-package avy
  :bind (("M-g 1" . avy-goto-char)
         ("M-g 2" . avy-goto-char-2)
         ("M-g t" . avy-goto-char-timer)
         ("M-g f" . avy-goto-char-in-line)
         ("M-g l" . avy-goto-line)
         ("M-g s" . avy-goto-symbol-1)
         ("M-g 0" . avy-goto-word-0)
         ("M-g w" . avy-goto-word-1)))

(use-package auto-yasnippet
  :bind (("M-g c" . aya-create)
         ("M-g e" . aya-expand)))


;; 使用主题
(use-package solarized-theme
  :if window-system
  :config
  (load-theme 'solarized-light t))


;; yasnippet
(use-package yasnippet-snippets
  :if window-system
  :commands (yas-expand-snippet yas-insert-snippet yas-new-snippet)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :init
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x"     . counsel-M-x)
         ("M-y"     . counsel-yank-pop)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-h S"   . counsel-info-lookup-symbol)
         ("C-h a"   . counsel-apropos)
         ("C-M-s"   . counsel-grep-or-swiper)
         ("C-x b"   . ivy-switch-buffer)
         :map counsel-find-file-map
         ("C-l" . counsel-up-directory)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (progn
    (ivy-mode 1)
    (use-package wgrep)
    (use-package smex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit
;; 在 a-string 两边加上 " 或者 (), 只要将光标放置于 a-string 开头按下 M-( 或者 M-" 即可
;; (hello world) 光标放置于 hello world 中间按下 M-S 即可将其分割成 (hello) (world)
;; 按下 M-J 可以将其重新连接起来， 字符串也一样
;; C-(, C-) 吃掉左边或者右边的 s-exp, C-{, C-} 吐出来
;; M-r 跳出外围块(去掉外层代码)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package paredit
  :hook ((scheme-mode  lisp-mode emacs-lisp-mode inferior-lisp-mode geiser-repl-mode) . enable-paredit-mode))

(use-package key-chord
  :config
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "df" 'hydra-esc/body)))
