(use-modules (gi)
             (system foreign)
             (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

;; Testing glib's malloc is dumb, but, whatever.

;; I have no idea what the memory and GC ramifications are to all
;; this.  It might be fun to figure out.

(define SIZ MAXINT16)

(automake-test
 (let ((memptr (malloc SIZ)))
   (let ((bv (pointer->bytevector memptr SIZ)))

     ;; Will we segfault if we write into this space?
     (bytevector-u8-set! bv 0 123))

   ;; What happens when we free the bytevector?
   (gc)

   ;; Or what happens here?
   (free memptr)

   ;; If we made it here, success?
   #t))
