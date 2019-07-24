(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (array-fixed-out)))
   (format #t "Output: ~S~%" x)
   (list= eqv? '(-1 0 1 2) (int-vector->list x))))
