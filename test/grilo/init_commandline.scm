(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Grl" "0.3"))

(define (args-for-init args)
  (values (length args) args))

(automake-test
 ((compose init args-for-init)
  (command-line)))
