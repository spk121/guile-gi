(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (or
  (string=? SEARCHPATH_SEPARATOR_S ":")
  (string=? SEARCHPATH_SEPARATOR_S ";")))
