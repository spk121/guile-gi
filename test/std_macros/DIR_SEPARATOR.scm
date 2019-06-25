(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (or
  (equal? DIR_SEPARATOR (char->integer #\/))
  (equal? DIR_SEPARATOR (char->integer #\\))))
