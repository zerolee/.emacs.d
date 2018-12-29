(fset 'mytab
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item '([tab] 0 "%d") arg)))
(fset 'my-eval-last-sexp
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item '("" 0 "%d") arg)))
