(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (is-a? (main-loop:new #f #t) <GMainLoop>))
