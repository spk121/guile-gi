(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (= 2 VERSION_MIN_REQUIRED))
