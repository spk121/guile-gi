(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (boolean-out-true)))
   (format #t "Output: ~S~%" x)
   x))

