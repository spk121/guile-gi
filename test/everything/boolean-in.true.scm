(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((flag #t))
   (format #t "Input: ~S~%" flag)
   (boolean-in-true flag)
   #t))
