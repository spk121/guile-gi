(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (garray-uint64-none-return)))
   (format #t "Output: ~S~%" x)
   (list= eqv? '(0 #xFFFFFFFFFFFFFFFF) (u64vector->list x))))
