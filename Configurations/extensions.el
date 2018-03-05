;; sams-lib
(autoload 'sams-cm-save-point "sams-lib")

(use-package solarized-theme           :ensure t)
(use-package cquery                    :ensure t)
(use-package ht                        :ensure t)
(use-package wgrep                     :ensure t)
(use-package geiser                    :ensure t)
(use-package markdown-mode             :ensure t)
(use-package company-lsp               :ensure t)
(use-package ivy-xref                  :ensure t)
(use-package smex                      :ensure t)
(use-package yasnippet-snippets        :ensure t)
(use-package groovy-mode               :ensure t)


;; yasnippet
(use-package yasnippet
  :after company
  :ensure t
  :config
  (progn
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :ensure counsel
  :config
  (progn
    (ivy-mode 1)
    ; 将最近的文件和书签加入到 ivy-switch-buffer
    (setq ivy-use-virtual-buffers t)))

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
  :config
  (progn
    (add-hook 'scheme-mode-hook 'enable-paredit-mode)
    (add-hook 'elisp-mode-hook 'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'common-lisp-mode-hook 'enable-paredit-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'evil)
;;(evil-mode 1)
;;(setq evil-default-state 'emacs)
