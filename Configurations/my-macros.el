;;if(expression)<---此处按下C-o 结果如下(POINT means point of emacs)
;;if(expression){
;;   (>>>POINT<<<)
;;}					
(fset 'cool-newline
   "{}\C-b\C-j\C-p\C-e\C-j")
(global-set-key (kbd "C-o") 'cool-newline)


;; use printf to debug program, fflush can force buffer write data,
;; even it have not full.
(fset 'use-printf-debug
   "printf(\"something or other\");\C-jfflush(stdout);\C-j")
(global-set-key (kbd "s-p") 'use-printf-debug)

