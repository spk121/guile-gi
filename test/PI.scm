(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (and
  (> PI 3.1)
  (< PI 3.2)))
