(use-modules (gi)
             (test automake-test-lib))

(automake-test
 (begin
   (typelib-load "Gio" "2.0")
   (let ((app (make-gobject (get-gtype <GApplication>)
                            '(("application-id" . "gi.guile.Example")))))
     (modify-signals app
       (add-before activate
         (lambda (app)
           (display "Hello, world")
           (newline)
           (with-object app (quit)))))
     (with-object app (run (length (command-line)) (command-line))))))
