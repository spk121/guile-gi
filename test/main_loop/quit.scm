(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (let ((mainloop (main-loop:new #f #t)))
   (not (with-object mainloop (quit) (is-running?)))))
