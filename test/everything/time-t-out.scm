(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((x (time-t-out)))
   (write x)
   (newline)
   (= x 1234567890)))
