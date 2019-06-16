(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (and
  (> E 2.7)
  (< E 2.8)))
