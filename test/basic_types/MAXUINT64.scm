(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (= MAXUINT64 #xFFFFFFFFFFFFFFFF))
