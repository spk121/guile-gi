((c-mode (c-file-style . "stroustrup"))
 (scheme-mode
  (eval . (put 'and-let* 'scheme-indent-function 1))
  (eval . (put 'eval-when 'scheme-indent-function 1))
  (eval . (put 'match 'scheme-indent-function 1))

  (eval . (put 'test-assert 'scheme-indent-function 1))
  (eval . (put 'test-equal 'scheme-indent-function 1))
  (eval . (put 'test-eq 'scheme-indent-function 1))
  (eval . (put 'test-eqv 'scheme-indent-function 1))
  (eval . (put 'test-error 'scheme-indent-function 1))))
