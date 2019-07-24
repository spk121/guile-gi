(use-modules (gi)
             (ice-9 receive)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (receive (x sum)
     (array-return-etc 20 21)
   (format #t "Array: ~S~%" x)
   (format #t "Sum: ~s~%" sum)
   (and (list= eqv? '(20 0 1 21) (int-vector->list x))
        (= sum 41))))
