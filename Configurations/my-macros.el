(fset 'lzl-cool-newline
   "{}\C-b\C-j\C-p\C-e\C-j")

;;; 直接打开一个 dired
(fset 'lzl-dired
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item '([134217848 100 105 114 101 100 return return] 0 "%d") arg)))
