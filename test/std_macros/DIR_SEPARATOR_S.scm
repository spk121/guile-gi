(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (or
  (string=? DIR_SEPARATOR_S "/")
  (string=? DIR_SEPARATOR_S "\\")))
