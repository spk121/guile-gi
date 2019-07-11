(use-modules (gi)
             (rnrs bytevectors)
             (srfi srfi-1)
             (ice-9 receive)
             (system foreign)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

;; Convert a bytevector containing a Latin-1 string into
;; a bytevector containg a UTF8 string.

(automake-test
 (receive (pointer bytes-read bytes-written)
     (convert #vu8(193 201) ; Latin-1 encoded ÁÉ
              "UTF-8"
              "ISO-8859-1")
   (let ((converted (pointer->bytevector pointer bytes-written)))
     (format #t "converted: ~S~%" converted)
     (format #t "bytes-read: ~S~%" bytes-read)
     (format #t "bytes-written: ~S~%" bytes-written)
     (equal? converted
             #vu8(195 129           ; UTF-8 encoded Á
                      195 137))       ; UTF-8 encoded É
     )))
