(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(automake-test
 (let ((x (const-return-gdouble)))
   (write x)
   (newline)
   (eqv? x 0.0)))
