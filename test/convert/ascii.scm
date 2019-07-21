(use-modules (gi)
             (rnrs bytevectors)
             (system foreign)
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
              "UTF-8"
              "US-ASCII")
     (format #t "converted: ~S~%" converted)
     (format #t "bytes-read: ~S~%" bytes-read)
     (list= = (u8vector->list converted)
            '(65 66 67 68 69) ; UTF-8 'ABCDE'
            )))

