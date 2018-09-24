(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq load-path
      (cons "~/.emacs.d/Configurations"
            (cons "~/.emacs.d/Extensions" load-path)))
(let ((default-directory (file-name-as-directory "~/.emacs.d/elpa")))
  (normal-top-level-add-subdirs-to-load-path))

(setq package-enable-at-startup nil)

(run-with-idle-timer
 0 nil
 #'(lambda ()
     (with-temp-message ""
       (require 'yasnippet)
       (yas-reload-all)
       (add-hook 'prog-mode-hook #'yas-minor-mode)
       (require 'recentf)
       (recentf-mode 1)
       (require 'counsel)
       (require 'multiple-cursors)
       (require 'eglot))))
