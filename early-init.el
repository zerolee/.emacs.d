(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(run-with-idle-timer
 0 nil
 #'(lambda ()
     (with-temp-message ""
       (require 'recentf)
       (recentf-mode 1))))
