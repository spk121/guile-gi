(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (let ((mainloop (main-loop:new #f #t)))
   (send mainloop (is-running?))))
