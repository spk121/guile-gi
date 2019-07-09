(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (and
  (> E 2.7)
  (< E 2.8)))
