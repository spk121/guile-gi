(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (and
  (> PI 3.1)
  (< PI 3.2)))
