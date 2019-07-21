(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x #(#t #f #t #t)))
   (format #t "Input: ~S~%" x)
   (array-bool-in x)
   #t))

