;;; -*- lexical-binding: t; -*-
(require 'zerolee-lib)
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
  :init
  (defconst zerolee--ove-not-startup-mode
    '(treemacs-mode package-menu-mode ibuffer-mode bookmark-bmenu-mode
                    process-menu-mode occur-mode apropos-mode)
    "ove 默认不启动的 mode")
  :bind (("<escape>" . (lambda () (interactive) (ove-mode 1)))
         ("C-w" . (lambda () (interactive)
                    (if (use-region-p)
                        (kill-region (region-beginning) (region-end))
                      (ove-ckm "c"))))
         ("M-w" . (lambda () (interactive)
                    (if (use-region-p)
                        (kill-ring-save (region-beginning) (region-end))
                      (ove-ckm "m")))))
  :commands (ove-mode ove-ckm)
  :hook ((prog-mode text-mode comint-mode special-mode)
         .
         (lambda ()
           (if (and buffer-read-only
                    (not (memq major-mode zerolee--ove-not-startup-mode))
                    (not (derived-mode-p 'magit-mode)))
               (ove-mode 1)
             (setq cursor-type 'bar)))))

(use-package hugomd
  :ensure nil
  :commands hugomd-preview)

(use-package diminish)
(use-package hydra)

(use-package projectile
  :defer t
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (advice-add 'projectile-find-file-dwim :before
              #'(lambda (&rest _)
                  (xref-push-marker-stack)
                  (ove-mode 1))))


(use-package mc-mark-more
  :ensure multiple-cursors
  :init
  (global-set-key (kbd "M-g m") 'mc/edit-lines)
  :bind (("M-g r" . mc/mark-all-in-region-regexp)
         ("M-g a" . mc/mark-all-like-this)
         ("M-g W" . mc/mark-next-like-this-word)
         ("M-g S" . mc/mark-next-like-this-symbol)
         ("M-g ." . mc/mark-all-dwim)
         ("C-M-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("M-[" . mc/mark-previous-like-this)
         ("M-]" . mc/unmark-previous-like-this)
         ("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/unmark-next-like-this)
         ("M-s" . mc/skip-to-next-like-this)
         ("M-g" . mc/skip-to-previous-like-this)
         ("M-i" . mc/insert-numbers)
         ("M-." . mc/mark-all-like-this-dwim)))

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
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        recentf-max-saved-items 100
        recentf-exclude '("/tmp/" "/ssh:" "/su\\(do\\)?:" "\.gz$" "\.elc$"
                          "COMMIT_EDITMSG" "/elpa/" "\.gitignore" "README"
                          "/usr/" "cache" "backup" "TODO" "ChangeLog"
                          "\.pls$" "\.m3u$" "\.lrc$" "\.LRC$" "\.dpl" "\.dat"
                          "bookmarks" "VERSION" "emms-history")
        counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (with-eval-after-load 'ivy
    (require 'counsel)
    (require 'ove)
    (require 'dumb-jump))
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x"     . (lambda () (interactive)
                        (call-process "fcitx-remote" nil nil nil "-c")
                        (counsel-M-x)))
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
    (use-package smex)
    (use-package ivy-xref
      :init
      (setq xref-show-definitions-function #'ivy-xref-show-defs
            xref-show-xrefs-function #'ivy-xref-show-xrefs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0)
  :config
  (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
    "Enable yasnippet but disable it inline."
    (if (eq command 'prefix)
        (when-let ((prefix (funcall fun 'prefix)))
          (unless (memq (char-before (- (point) (length prefix))) '(?. ?> ?\())
            prefix))
      (funcall fun command arg)))
  (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit
;; 在 a-string 两边加上 " 或者 (), 只要将光标放置于 a-string 开头按下 M-( 或者 M-" 即可
;; (hello world) 光标放置于 hello world 中间按下 M-S 即可将其分割成 (hello) (world)
;; 按下 M-J 可以将其重新连接起来， 字符串也一样
;; C-(, C-) 吃掉左边或者右边的 s-exp, C-{, C-} 吐出来
;; M-r 跳出外围块(去掉外层代码)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package paredit
  :hook ((scheme-mode  lisp-mode emacs-lisp-mode
                       inferior-lisp-mode geiser-repl-mode sly-mrepl-mode)
         . enable-paredit-mode)
  :config
  (setq paredit-lighter nil)
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  (define-key paredit-mode-map (kbd "M-?") nil)
  (define-key paredit-mode-map (kbd "M-<up>") 'paredit-splice-sexp)
  (define-key paredit-mode-map (kbd "M-<down>")
    '(lambda ()
       (interactive)
       (search-forward ")" (point-at-eol) t 1)
       (paredit-newline)
       (ove-mode 0)))
  (define-key paredit-mode-map (kbd "(") nil)
  (define-key paredit-mode-map (kbd ")") nil)
  (define-key paredit-mode-map (kbd "[") nil)
  (define-key paredit-mode-map (kbd "]") nil)
  (define-key paredit-mode-map (kbd ";") nil)
  (advice-add 'paredit-comment-dwim :after
              #'(lambda (&optional _) (unless mark-active
                                        (ove-mode 0)))))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "df" '(lambda () (interactive) (ove-mode 1))))

(use-package emms :defer t)

(use-package my-emms
  :ensure nil
  :commands (zerolee-emms-default zerolee-emms-favourite))

(use-package my-tools
  :ensure nil
  :commands (zerolee-eshell zerolee-find-file zerolee-compile
                            zerolee-rg zerolee-go zerolee-open-with
                            zerolee-delete-window))

(use-package magit :defer t)

(use-package remember
  :ensure nil
  :custom
  (remember-data-file "~/.emacs.d/notes.org")
  :bind ("<C-f5>" . remember))
