(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package save-position
  :ensure nil
  :bind (("s-." . sp-push-position-to-ring)
         ("s-," . sp-get-position-from-ring)
         ("s-/" . sp-show-all-position-in-ring)))

(use-package ove
  :ensure nil
  :bind ("<escape>" . (lambda () (interactive)
                        (ove-mode 1)))
  :hook ((prog-mode text-mode comint-mode special-mode)
         .
         (lambda () (interactive)
           (if (and buffer-read-only
                    (not (equal major-mode 'treemacs-mode)))
               (ove-mode 1)
             (setq cursor-type 'bar)))))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package hydra)

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy))

(use-package goto-chg
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package mc-mark-more
  :ensure multiple-cursors
  :init
  (global-set-key (kbd "M-g m") 'mc/edit-lines)
  :bind (("M-g r" . mc/mark-all-in-region-regexp)
         ("M-g a" . mc/mark-all-like-this)
         ("M-g W" . mc/mark-next-like-this-word)
         ("M-g S" . mc/mark-next-like-this-symbol)
         ("C-M-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("M-[" . mc/mark-previous-like-this)
         ("M-]" . mc/unmark-previous-like-this)
         ("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/unmark-next-like-this)
         ("M-s" . mc/skip-to-next-like-this)
         ("M-g" . mc/skip-to-previous-like-this)
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

(use-package avy
  :bind (("M-g 1" . avy-goto-char)
         ("M-g 2" . avy-goto-char-2)
         ("M-g t" . avy-goto-char-timer)
         ("M-g f" . avy-goto-char-in-line)
         ("M-g g" . avy-goto-line)
         ("M-g s" . avy-goto-symbol-1)
         ("M-g 0" . avy-goto-word-0)
         ("M-g w" . avy-goto-word-1)))

;; 使用主题
(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))


;; yasnippet
(use-package yasnippet-snippets
  :commands (yas-expand-snippet yas-insert-snippet yas-new-snippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :init
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        recentf-max-saved-items 100
        recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "\.gz$" "\.elc$"
                          "COMMIT_EDITMSG" "/elpa/" "\.gitignore" "README"
                          "/usr/" "cache" "backup")
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
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit
;; 在 a-string 两边加上 " 或者 (), 只要将光标放置于 a-string 开头按下 M-( 或者 M-" 即可
;; (hello world) 光标放置于 hello world 中间按下 M-S 即可将其分割成 (hello) (world)
;; 按下 M-J 可以将其重新连接起来， 字符串也一样
;; C-(, C-) 吃掉左边或者右边的 s-exp, C-{, C-} 吐出来
;; M-r 跳出外围块(去掉外层代码)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package paredit
  :hook ((scheme-mode  lisp-mode emacs-lisp-mode inferior-lisp-mode geiser-repl-mode) . enable-paredit-mode)
  :config
  (setq paredit-lighter nil)
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  (define-key paredit-mode-map (kbd "M-?") nil)
  (define-key paredit-mode-map (kbd "M-<up>") 'paredit-splice-sexp)
  (define-key paredit-mode-map (kbd "M-<down>") '(lambda ()
                                                   (interactive)
                                                   (lzl-look-forward-char 1 ?\))
                                                   (paredit-newline)
                                                   (ove-mode 0)))
  (define-key paredit-mode-map (kbd "(") nil)
  (define-key paredit-mode-map (kbd ")") nil)
  (define-key paredit-mode-map (kbd "[") nil)
  (define-key paredit-mode-map (kbd "]") nil)
  (define-key paredit-mode-map (kbd ";") nil)
  (advice-add 'paredit-comment-dwim :after
              #'(lambda (&optional arg) (unless mark-active
                                          (ove-mode 0)))))

(use-package cmake-mode)

(use-package key-chord
  :config
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "df" '(lambda () (interactive) (ove-mode 1)))))
