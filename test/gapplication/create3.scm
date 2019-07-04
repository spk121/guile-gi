(use-modules (gi)
             (test automake-test-lib))

(automake-test
 (begin
   (typelib-load "Gio" "2.0")
   (with-object (create <GApplication>
                  (application-id "gi.guile.Example"))
     (connect! activate
       (lambda (app)
         (display "Hello, world")
         (newline)
         (with-object app (quit))))
     (run (length (command-line)) (command-line)))))
