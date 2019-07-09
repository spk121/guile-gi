(use-modules (gi)
             (rnrs bytevectors)
             (srfi srfi-1)
             (ice-9 receive)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

;; Convert a bytevector containing an ascii string into
;; a bytevector containg a UTF8 string.  Should be
;; unchanged.

(automake-test
 (receive (converted bytes-read bytes-written)
     (convert #vu8(65 66 67 68 69) ; ASCII 'ABCDE'
              5
              "UTF-8"
              "US-ASCII")
   (equal? (take (bytevector->u8-list converted) bytes-written)
           '(65 66 67 68 69)) ; UTF-8 'ABCDE'
   ))
