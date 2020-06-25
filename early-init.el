;; -*- coding: utf-8; lexical-binding: t; -*-
(dolist (feature '(menu tool scroll))
  (funcall (intern (format "%s-bar-mode" feature)) -1))
(run-with-idle-timer
 0 nil
 #'(lambda ()
     (with-temp-message ""
       (require 'recentf)
       (recentf-mode 1))))
