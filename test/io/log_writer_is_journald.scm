(use-modules (gi)
             (ice-9 hash-table)
             (system foreign)
             (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (not (log-writer-is-journald? (fileno (current-error-port)))))
