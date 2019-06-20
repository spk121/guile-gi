(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib))

;; Another curious case.  The first argument is supposed to be a
;; zero-terminated UTF32 string that is modified in place.  The
;; introspection info list the 1st argument as IN.  What's the right
;; approach?

(automake-test
 (let* ((instr (apply string (list #\A #\◌́ #\◌̱)))
        (inbv (string->utf32 instr))
        ;; (out (unicode-canonical-ordering inbv 3))
        )
   (write instr) (newline)
   (write inbv) (newline)
   ;; (write out) (newline)
   'skipped))
