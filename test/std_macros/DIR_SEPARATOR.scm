(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (or
  (equal? DIR_SEPARATOR (char->integer #\/))
  (equal? DIR_SEPARATOR (char->integer #\\))))
