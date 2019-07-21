(use-modules (gi)
             (test automake-test-lib)
             (ice-9 receive)
             (srfi srfi-1)
             (srfi srfi-43))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (list->int-vector '(-1 0 1 2))))
   (format #t "Input Before: ~S~%" x)
   (receive (sum y)
       (array-inout-etc 100 x 101)
     (format #t "Input After: ~S~%" x)
     (format #t "Output: ~S~%" y)
     (format #t "Sum: ~S~%" sum)
     (and (list= eqv? '(100 -1 0 1 101) (int-vector->list y))
          (= sum 201)))))
