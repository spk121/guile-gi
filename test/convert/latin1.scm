(use-modules (gi)
             (rnrs bytevectors)
             (srfi srfi-1)
             (ice-9 receive)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

;; Convert a bytevector containing a Latin-1 string into
;; a bytevector containg a UTF8 string.

(automake-test
 (receive (converted bytes-read bytes-written)
     (convert #vu8(193 201) ; Latin-1 encoded ÁÉ
              2
              "UTF-8"
              "ISO-8859-1")
   (equal? (take (bytevector->u8-list converted) bytes-written)
           '(195 129           ; UTF-8 encoded Á
                 195 137       ; UTF-8 encoded É
           ))))
