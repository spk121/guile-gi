(use-modules (gi)
             (test automake-test-lib)
             (rnrs bytevectors)
             (system foreign))

(typelib-require ("Everything" "1.0"))

;; Assuming 64-bit pointers

(automake-test
 (let* ((bv (make-bytevector 1))
        (inptr (bytevector->pointer bv))
        (outptr (passthrough-one-gpointer inptr)))
   (write bv) (newline)
   (write inptr) (newline)
   (write outptr) (newline)
   (equal? inptr outptr)))
