(use-modules (gi) (gi repository)
             (test automake-test-lib))

(unless (false-if-exception (require "Gio" "2.0"))
  (exit EXIT_SKIPPED))

(load-by-name "Gio" "Application")

(automake-test
 (begin
   (let ((app (make <GApplication>
                #:application-id "gi.guile.Example")))
     (connect app activate
              (lambda (app)
                (display (application-id app))
                (display " says \"Hello, world!\"")
                (newline)
                (quit app)))

     (run app (command-line)))))
