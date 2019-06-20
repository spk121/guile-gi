(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (unichar-validate? #x0000))
