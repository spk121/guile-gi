(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib))

;; At the moment this test fails on 64-bit platforms because the
;; bytevector is supposed to be being passed as a pointer, but, it is
;; being passed as a pointer truncated to 32-bits.  This might be an
;; upstream bug.
(automake-test
 (let ((bv (make-bytevector 8 ; bytes
                            0 ; value
                            )))
   (bytevector-u64-native-set! bv 0 #x0123456789abcdef)
   ;; Why does atomic-int-set received 'bv' as a 32-bit pointer?
   (atomic-int-set bv 1)
   (equal? (bytevector-s32-native-ref bv 0) 1)))

