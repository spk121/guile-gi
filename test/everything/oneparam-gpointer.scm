(use-modules (gi)
             (test automake-test-lib)
             (rnrs bytevectors)
             (system foreign))

(typelib-require ("Everything" "1.0"))

(automake-test
 (let ((bv (make-bytevector 1 0)))
   (oneparam-gpointer (bytevector->pointer bv))
   #t))
