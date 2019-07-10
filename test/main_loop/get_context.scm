(use-modules (gi)
             (oop goops)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (let ((mainloop (main-loop:new #f #t)))
   (let ((ctx (with-object mainloop (get-context))))
     (is-a? ctx <GMainContext>))))
