(use-modules (gi)
             (system foreign)
             (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

;; Allocate way too much memory
(define SIZ MAXINT64)

(automake-test
 (let ((memptr (try-malloc0 SIZ)))
   ;; Unless you are a supercomputer, memptr should be #f for failure.
   (write memptr) (newline)
   (not memptr)))
