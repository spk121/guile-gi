(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0") ("Gio" "2.0"))

(automake-test
 (begin
   (let ((app (make <GApplication>
                #:application-id "gi.guile.Example")))
     (connect app (make <signal> #:name "activate")
              (lambda (app)
                (display (gobject-get-property app "application-id"))
                (display " says \"Hello, world!\"")
                (newline)
                (quit app)))

     (run app (command-line)))))
