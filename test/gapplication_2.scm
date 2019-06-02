(use-modules (gi)
             (test automake-test-lib))

(automake-test
 (begin
   (typelib-load "Gio" "2.0")
   (let ((app (make-gobject <GApplication>
                            '(("application-id" . "gi.guile.Example")))))
     (connect app (activate
                   (lambda (app user-data)
                     (display "Hello, world")
                     (newline)
                     (send app (quit)))))
     (send app (run (length (command-line)) (command-line))))))
