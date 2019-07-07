(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (or
  (equal? SEARCHPATH_SEPARATOR (char->integer #\:))
  (equal? SEARCHPATH_SEPARATOR (char->integer #\;))))
