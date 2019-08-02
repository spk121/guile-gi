(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (list->u64vector '(0 #xFFFFFFFFFFFFFFFF))))
   (format #t "Input: ~S~%" x)
   (garray-uint64-none-in x)
   #t))
