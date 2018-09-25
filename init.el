(setq load-path
      (cons "~/.emacs.d/Configurations"
            (cons "~/.emacs.d/Extensions" load-path)))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq gc-cons-threshold 100000000
      file-name-handler-alist nil)
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(setq custom-file (expand-file-name "Configurations/custom.el" user-emacs-directory))

(load "emacs-std")
(load "extensions")
(load "modes")
(load "my-macros")
(load "hydras")
(load "bindings")
(load-file custom-file)
