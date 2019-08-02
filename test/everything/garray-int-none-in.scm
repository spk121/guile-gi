(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (list->int-vector '(-1 0 1 2))))
   (format #t "Input: ~S~%" x)
   (garray-int-none-in x)
   #t))
