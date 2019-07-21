(use-modules (gi)
             (test automake-test-lib)
             (ice-9 receive)
             (srfi srfi-1))


(typelib-require ("Marshall" "1.0"))

(automake-test
 (receive (x sum)
     (array-out-etc 20 21)
   (format #t "Output: ~S~%" x)
   (format #t "Sum: ~S~%" sum)
   (and (list= eqv? (int-vector->list x) '(20 0 1 21))
        (= sum 41))))
