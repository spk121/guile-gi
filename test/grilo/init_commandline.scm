(use-modules (gi)
             (test automake-test-lib))

(unless (false-if-exception (typelib-load "Grl" "0.3"))
  (exit EXIT_SKIPPED))

(define (args-for-init args)
  (values (length args) args))

(automake-test
 ((compose init args-for-init)
  (command-line)))
