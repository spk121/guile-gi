(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib))

(automake-test
 (str-has-prefix? "ABCDEFG" "ABC"))

