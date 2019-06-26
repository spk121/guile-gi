(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (gmain-loop? (main-loop:new #f #t)))
