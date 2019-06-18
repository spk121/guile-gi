(use-modules (gi) (gi glib-2)
             (ice-9 hash-table)
             (system foreign)
             (rnrs bytevectors)
             (test automake-test-lib))

;; FIXME: What the test is intended to do is set up a log handler, and
;; then write to the log.  But the only introspected log writing
;; procedure is 'log-structured-array', and that requires directly
;; setting the fields of a structure.

(define (my-logger domain level message user-data)
  (format #t "LOG DOMAIN ~s, LEVEL ~S, MESSAGE ~S, USER_DATA ~S~%"
          domain level message user-data))

(define (destroy-notify user-data)
  (format #t "DESTROY NOTIFY ~S~%" user-data))

(automake-test
 (let ((ID (log-set-handler #f
                            (logior LOG_LEVEL_LEVEL_WARNING
                                    LOG_LEVEL_FLAG_FATAL
                                    LOG_LEVEL_FLAG_RECURSION)
                            my-logger
                            #f
                            destroy-notify)))

   ;; FIXME: here one would write to the log using
   ;; log-structured-array.
   'skipped))
