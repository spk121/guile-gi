(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (or
  (string=? SEARCHPATH_SEPARATOR_S ":")
  (string=? SEARCHPATH_SEPARATOR_S ";")))
