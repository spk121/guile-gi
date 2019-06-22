(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (system foreign)
             (srfi srfi-1)
             (test automake-test-lib))

;; Convert a bytevector containing an ascii string into
;; a bytevector containg a UTF8 string.  Should be
;; unchanged.

(automake-test
 (let ((input (convert (u8-list->bytevector '(65 66 67 68 69)) ; ASCII 'ABCDE'
                       5
                       "UTF-8"
                       "US-ASCII")))
   (let ((output (take (bytevector->u8-list (first input))
                       (third input))))
     (equal? output '(65 66 67 68 69)   ; UTF-8 'ABCDE'
             ))))
