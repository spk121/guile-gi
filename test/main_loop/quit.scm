(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (not
  (let ((mainloop (main-loop:new #f #t)))
    (quit mainloop)
    (is-running? mainloop))))
