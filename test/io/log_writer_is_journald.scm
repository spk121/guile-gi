(use-modules (gi) (gi glib-2)
             (ice-9 hash-table)
             (system foreign)
             (rnrs bytevectors)
             (test automake-test-lib))

(automake-test
 (not (log-writer-is-journald? (fileno (current-error-port)))))
