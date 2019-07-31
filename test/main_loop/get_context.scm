(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (let ((mainloop (main-loop:new #f #t)))
   (is-a? (get-context mainloop) <GMainContext>)))
