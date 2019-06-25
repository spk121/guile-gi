(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (integer? MAJOR_VERSION))
