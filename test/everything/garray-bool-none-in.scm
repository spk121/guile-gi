(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x #(#t #f #t #t)))
   (format #t "Input: ~S~%" x)
   (garray-bool-none-in x)
   #t))
