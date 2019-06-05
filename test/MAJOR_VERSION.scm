(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (= MAJOR_VERSION 2))
