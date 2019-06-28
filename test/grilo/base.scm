(define-module (test grilo base)
  #:use-module (gi)
  #:use-module (test automake-test-lib)
  #:re-export (send
               EXIT_SUCCESS
               EXIT_FAILURE
               automake-test))

(unless (false-if-exception
         (begin
           (typelib-load "Grl" "0.3")
           (init 0 #f)
           #t))
  (exit EXIT_SKIPPED))
