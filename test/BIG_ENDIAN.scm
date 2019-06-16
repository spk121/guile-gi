(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (= BIG_ENDIAN 4321))

