;; init-macros.el --- 用以保存一些键盘宏 -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(fset 'mytab
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item '([tab] 0 "%d") arg)))
(fset 'my-eval-last-sexp
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item '("" 0 "%d") arg)))

(provide 'init-macros)
;;; init-macros.el ends here
