((c-mode (c-file-style . "stroustrup"))
 (scheme-mode
  (eval . (put 'connect! 'scheme-indent-function 1))
  (eval . (put 'connect-after! 'scheme-indent-function 1))
  (eval . (put 'with-object 'scheme-indent-function 1))
  (eval . (put 'create 'scheme-indent-function 1))))
