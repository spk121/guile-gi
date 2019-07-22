(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0") ("Gio" "2.0"))

(automake-test
 (begin
   (let ((app (make <GApplication>
                #:application-id "gi.guile.Example")))
     (connect app activate
              (lambda (app)
                (display (application:application-id app))
                (display " says \"Hello, world!\"")
                (newline)
                (quit app)))

     (run app (command-line)))))
