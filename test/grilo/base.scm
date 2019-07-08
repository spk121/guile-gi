(define-module (test grilo base)
  #:use-module (gi)
  #:use-module (test automake-test-lib)
  #:re-export (with-object
               EXIT_SUCCESS
               EXIT_FAILURE)
  #:export (grilo-test))

(define-syntax-rule (grilo-test x)
  (automake-test
   (begin
     (typelib-require ("Grl" "0.3"))
     (init)
     x)))
