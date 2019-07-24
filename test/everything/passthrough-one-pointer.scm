(use-modules (gi)
             (test automake-test-lib)
             (rnrs bytevectors)
             (system foreign))

(typelib-require ("Everything" "1.0"))

;; Assuming 64-bit pointers

(automake-test
 (let ((bv (make-bytevector 8)))
   (bytevector-u64-set! bv 0 #x12345678 (native-endianness))
   (write bv) (newline)
   (passthrough-one-gpointer (bytevector->pointer bv))
   (write bv) (newline)
   (= 0 (bytevector-u64-ref bv 0))))
