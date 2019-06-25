(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (= LITTLE_ENDIAN 1234))
