(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (or
  (string=? DIR_SEPARATOR_S "/")
  (string=? DIR_SEPARATOR_S "\\")))
