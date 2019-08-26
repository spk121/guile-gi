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
 (if (< MINOR_VERSION 56)
     'skipped
     (receive (converted bytes-read)
	 (convert #vu8(193 201) ; Latin-1 encoded ÁÉ
		  "UTF-8"
		  "ISO-8859-1")
       (format #t "converted: ~S~%" converted)
       (format #t "bytes-read: ~S~%" bytes-read)
       (list= = (u8vector->list converted)
	      '(195 129           ; UTF-8 encoded Á
		    195 137))       ; UTF-8 encoded É
       )))
