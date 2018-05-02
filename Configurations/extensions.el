;; save-position
(autoload 'sp-push-position-to-ring "save-position")

(use-package wgrep                     :ensure t)
(use-package markdown-mode             :ensure t)
(use-package smex                      :ensure t)
(use-package hydra                     :ensure t)
(use-package avy                       :ensure t)
(use-package expand-region             :ensure t)
(use-package treemacs                  :ensure t)
(use-package iedit                     :ensure t)
(use-package projectile                :ensure t)
(use-package auto-yasnippet
  :commands (aya-create aya-expand)
  :ensure t)


;; 使用主题
(use-package solarized-theme
  :if window-system
  :ensure t
  :config
  (load-theme 'solarized-light t))


;; yasnippet
(use-package yasnippet-snippets
  :if window-system
  :ensure t
  :commands (yas-expand-snippet yas-insert-snippet yas-new-snippet)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t    ; 将最近的文件和书签加入到 ivy-switch-buffer
          ivy-use-selectable-prompt t
          counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never '%s' %s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
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
  :ensure t
  :hook ((scheme-mode  lisp-mode emacs-lisp-mode inferior-lisp-mode geiser-repl-mode) . enable-paredit-mode))

(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "df" 'hydra-esc/body)))
