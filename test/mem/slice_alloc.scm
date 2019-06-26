(use-modules (gi) (gi glib-2)
             (system foreign)
             (rnrs bytevectors)
             (test automake-test-lib))

(define SIZ 10)

(automake-test
 (let ((memptr (slice-alloc SIZ)))
   (let ((bv (pointer->bytevector memptr SIZ)))
     
     ;; Will we segfault if we write into this space?
     (write bv) (newline)
     (bytevector-u8-set! bv 0 123)
     (write bv)
     (newline))
   
   ;; What happens when we free the bytevector?
   (gc)

   ;; Or what happens here?
   (slice-free1 SIZ memptr)

   ;; If we made it here, success?
   #t))
