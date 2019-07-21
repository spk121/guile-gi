(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(automake-test
 (let ((x (const-return-gint32)))
   (write x)
   (newline)
   (eq? x 0)))
