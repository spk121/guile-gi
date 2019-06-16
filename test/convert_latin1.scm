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
   (let ((output (bytevector->u8-list
                  (pointer->bytevector
                   (first input)          ; a pointer
                   (third input)          ; number of output bytes
                   ))))
     (write output)
     (equal? output '(195 129           ; UTF-8 encoded Á
                          195 137       ; UTF-8 encoded É
                          )))))
