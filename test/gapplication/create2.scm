(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0") ("Gio" "2.0"))

(automake-test
 (begin
   (let ((app (make-gobject <GApplication>
                            '(("application-id" . "gi.guile.Example")))))
     (unless (equal? (gobject-get-property app "application-id")
                     "gi.guile.Example")
       (error "oops, something happened to our memory"))
     (with-object app
       (connect! activate
         (lambda (app)
           (display "Hello, world")
           (newline)
           (with-object app (quit))))
       (run (command-line))))))
