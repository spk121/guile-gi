(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (system foreign)
             (srfi srfi-1)
             (test automake-test-lib))

;; Convert a bytevector containing a Latin-1 string into
;; a bytevector containg a UTF8 string.

(automake-test
 (let ((input (convert (u8-list->bytevector '(193 ; Latin-1 encoded Á
                                              201)) ; Latin-1 encoded É
                       2
                       "UTF-8"
                       "ISO-8859-1")))
   ;; for some reason, the null byte is still included in the output
   (let ((output (take (bytevector->u8-list (first input))
                       (third input))))
     (write output)
     (equal? output '(195 129           ; UTF-8 encoded Á
                          195 137       ; UTF-8 encoded É
                          )))))
