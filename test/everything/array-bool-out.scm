(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-43))


(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (array-bool-out)))
   (format #t "Output: ~S~%" x)
   (vector= eqv? x #(#t #f #t #t))))
