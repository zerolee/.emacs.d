;; init-macros.el --- 用以保存一些键盘宏 -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(defalias 'kmacro/my-eval-last-sexp (kmacro "C-x C-e"))
(defalias 'kmacro/my-next-line (kmacro "C-n"))
(defalias 'kmacro/my-prev-line (kmacro "C-p"))

(provide 'init-macros)
;;; init-macros.el ends here
