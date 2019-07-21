(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (list->int-vector '(-1 0 1 2))))
   (format #t "Input Before: ~S~%" x)
   (let ((ret (array-fixed-inout x)))
     (format #t "Input After: ~S~%" x)
     (format #t "Output: ~S~%" ret)
     (list= eqv? '(2 1 0 -1) (int-vector->list ret)))))
