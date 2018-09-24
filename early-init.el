(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq package-enable-at-startup nil)
(run-with-idle-timer
 0 nil
 #'(lambda ()
     (with-temp-message ""
       (require 'yasnippet)
       (yas-reload-all)
       (add-hook 'prog-mode-hook #'yas-minor-mode))))


(run-with-idle-timer
 0 nil
 #'(lambda ()
     (with-temp-message
         (require 'recentf)
       (recentf-mode 1))))
