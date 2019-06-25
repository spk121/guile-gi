(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (unichar-isalpha? #\と))
