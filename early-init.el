;; -*- coding: utf-8; lexical-binding: t; -*-
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq gc-cons-threshold (* 128 1024 1024)
      file-name-handler-alist nil
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold (* 32 1024 1024)
                   gc-cons-percentage 0.3)
             (garbage-collect)) t)

(dolist (feature '(menu tool scroll))
  (funcall (intern (format "%s-bar-mode" feature)) -1))
(run-with-idle-timer
 0 nil
 #'(lambda ()
     (with-temp-message ""
       (require 'recentf)
       (recentf-mode 1))))

(provide 'early-init)
;;; early-init.el ends heres
