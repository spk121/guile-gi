(define-module (test grilo base)
  #:use-module (gi)
  #:use-module (test automake-test-lib)
  #:re-export (send
               EXIT_SUCCESS
               EXIT_FAILURE)
  #:export (grilo-test))

(define-syntax-rule (grilo-test x)
  (automake-test
   (if (not (false-if-exception
             (begin
               (typelib-load "Grl" "0.3")
               (init 0 #f)
               #t)))
       'skipped
       x)))
