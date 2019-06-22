(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (srfi srfi-1)
             (ice-9 receive)
             (test automake-test-lib))

;; Convert a bytevector containing a Latin-1 string into
;; a bytevector containg a UTF8 string.

(automake-test
 (receive (converted bytes-read bytes-written)
     (convert (u8-list->bytevector '(193 ; Latin-1 encoded Á
                                     201)) ; Latin-1 encoded É
                       2
                       "UTF-8"
                       "ISO-8859-1")

   (equal? (take (bytevector->u8-list converted) bytes-written)
           '(195 129           ; UTF-8 encoded Á
                 195 137       ; UTF-8 encoded É
           ))))
