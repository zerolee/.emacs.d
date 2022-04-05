;;; extensions.el --- 编程语言之外的第三方扩展相关配置 -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'zerolee-lib)
(use-package save-position
  :ensure nil
  :bind (("s-." . sp-push-position-to-ring)
         ("s-," . sp-get-position-from-ring)
         ("s-/" . sp-show-all-position-in-ring)))

(use-package vesie
  :ensure nil
  :init
  (defconst zerolee--vesie-not-startup-mode
    '(treemacs-mode package-menu-mode ibuffer-mode bookmark-bmenu-mode
                    process-menu-mode occur-mode apropos-mode proced-mode)
    "vesie 默认不启动的 mode")
  :bind (("<escape>" . (lambda () (interactive) (vesie-mode 1)))
         ("C-w" . (lambda () (interactive)
                    (if (use-region-p)
                        (kill-region (region-beginning) (region-end))
                      (vesie-ckm "c"))))
         ("M-w" . (lambda () (interactive)
                    (if (use-region-p)
                        (kill-ring-save (region-beginning) (region-end))
                      (vesie-ckm "m")))))
  :commands (vesie-mode vesie-ckm)
  :hook ((prog-mode text-mode comint-mode special-mode)
         .
         (lambda ()
           (if (and buffer-read-only
                    (not (memq major-mode zerolee--vesie-not-startup-mode))
                    (not (derived-mode-p 'magit-mode)))
               (vesie-mode 1)
             (setq cursor-type 'bar)))))

(use-package hugomd
  :ensure nil
  :commands hugomd-preview)

(use-package hydra)

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
  :hook (after-init . yas-global-mode)
  :bind (:map yas-minor-mode-map
              ("C-<tab>" . yas-next-field))
  :config
  (defun zerolee--autoinsert()
    "打开文件时从当前目录开始往上查找模板文件，查找到则插入模板.
若是没有查找到则到'~/模板/'目录下查找，找到则插入模板，否则不对文件做任何处理."
    (let* ((name (and (not (file-exists-p (buffer-file-name)))
                      (file-name-extension
                       (concat "arbitrary"
                               (file-name-nondirectory
                                (buffer-file-name))))))
           (template (and name (concat name "." name)))
           (place (and template
                       (or (locate-dominating-file default-directory template)
                           (and (member template (directory-files "~/模板"))
                                "~/模板/")))))
      (when place
        (insert-file-contents (concat place template))
        (yas-expand-snippet (buffer-string)
                            (point-min) (point-max)))))
  (add-hook 'find-file-hook #'zerolee--autoinsert))

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
                          "bookmarks" "VERSION" "emms-history" "\.jpg" "\.png"
                          "\.gif" "\.JPG" "\.webp" "\.jpeg" "\.JPEG" "snippets/")
        counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (with-eval-after-load 'ivy
    (require 'counsel)
    (require 'vesie)
    (require 'dumb-jump))
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x"     . (lambda () (interactive)
                        (zerolee-ime-disable)
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
         :map ivy-minibuffer-map
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
  (company-show-quick-access 'left)
  (company-backends
   '((company-yasnippet company-capf
                        company-dabbrev-code)
     company-files company-keywords
     company-dabbrev))
  :bind (:map company-active-map
              ("M-/" . company-other-backend)
              ("C-s" . company-filter-candidates))
  :config
  (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
    "Enable yasnippet but disable it inline."
    (if (eq command 'prefix)
        (when-let ((prefix (funcall fun 'prefix))
                   (ppss (syntax-ppss)))
          (unless (or (memq (char-before (- (point) (length prefix)))
                            '(?. ?> ?\( ?\) ?{ ?} ?\" ?' ?`))
                      (nth 3 ppss)
                      (nth 4 ppss))
            prefix))
      (funcall fun command arg)))
  (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)
  (advice-add #'company-dabbrev-code :around #'my-company-yasnippet-disable-inline)
  (setq company-transformers '(delete-dups))
  (add-hook 'company-completion-started-hook
            (lambda (_)
              (setq eldoc-echo-area-use-multiline-p 1)))

  (add-hook 'company-after-completion-hook
            (lambda (_)
              (setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit))))

(use-package flee
  :ensure nil
  :commands (flee-dwim))
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
  (defun paredit/my-next-parameter ()
    (interactive)
    (call-interactively #'up-list)
    (insert " ")
    (vesie-mode 0))
  (setq paredit-lighter nil)
  (zerolee-set-key
   paredit-mode-map
   ("M-s" nil) ("M-r" nil) ("M-?" nil) ("M-<up>" nil) ("(" nil) (")" nil)
   ("[" nil) ("]" nil) (";" nil)
   ("C-M-n" #'paredit/my-next-parameter)
   ("C-M-j" #'flee-dwim)
   ("<tab>"
    (lambda ()
      (interactive)
      (let ((point (point)))
        (call-interactively #'indent-for-tab-command)
        (when (= point (point))
          (if (or (and (eq (char-after) ?\)) (eq (char-before) ?\)))
                  (and (= (car (syntax-ppss)) 1) (eq (char-after) ?\))))
              (end-of-line)
            (when (and (/= (car (syntax-ppss)) 0)
                       (memql (char-after) '(?\) ?\")))
              (paredit/my-next-parameter)))))))
   ("M-i"
    (lambda ()
      (interactive)
      (paredit-backward-up
       (if (nth 3 (syntax-ppss)) 2 1))
      (forward-char 1))))
  (global-set-key (kbd "M-<up>") #'paredit-splice-sexp)
  (advice-add 'paredit-comment-dwim :after
              (lambda (&optional _) (unless mark-active
                                      (vesie-mode 0)))))

(use-package emms :defer t)

(use-package init-emms
  :ensure nil
  :commands (zerolee-emms-default))

(use-package init-tools
  :ensure nil
  :commands (zerolee-eshell zerolee-find-file zerolee-compile
                            zerolee-rg zerolee-go zerolee-open-with
                            zerolee-delete-window zerolee-goto-last-edit))

(use-package magit :defer t)

(provide 'extensions)
;;; extensions.el ends here
