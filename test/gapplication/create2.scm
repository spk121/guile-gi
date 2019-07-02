(use-modules (gi)
             (test automake-test-lib))

(automake-test
 (begin
   (typelib-load "Gio" "2.0")
   (let ((app (make-gobject <GApplication>
                            '(("application-id" . "gi.guile.Example")))))
     (unless (equal? (gobject-get-property app "application-id")
                     "gi.guile.Example")
       (error "oops, something happened to our memory"))
     (modify-signals app
       (add-before activate
         (lambda (app)
           (display "Hello, world")
           (newline)
           (with-object app (quit)))))
     (with-object app (run (length (command-line)) (command-line))))))
