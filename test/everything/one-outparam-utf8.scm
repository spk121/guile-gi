(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(automake-test
 (let ((x (one-outparam-utf8)))
   (write x) (newline)
   (string-null? x)))
