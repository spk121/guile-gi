(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (let ((mainloop (main-loop:new #f #t)))
   (with-object mainloop (is-running?))))
