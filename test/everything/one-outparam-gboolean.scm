(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(automake-test
 (let ((x (one-outparam-gboolean)))
   (write x) (newline)
   (eqv? #f x)))
