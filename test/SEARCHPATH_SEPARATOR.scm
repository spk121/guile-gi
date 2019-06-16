(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (or
  (equal? SEARCHPATH_SEPARATOR (char->integer #\:))
  (equal? SEARCHPATH_SEPARATOR (char->integer #\;))))
