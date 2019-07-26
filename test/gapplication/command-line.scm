(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-43))

(typelib-require ("Gio" "2.0"))

;; g_application_command_line_get_arguments returns a pointer to an
;; array whose length is one of the returned arguments.  Array
;; unpacking is not yet handled for this case.

(automake-test
 (begin
   (let ((app (make <GApplication>
                #:application-id "gi.guile.Example"
                #:flags APPLICATION_HANDLES_COMMAND_LINE))
         (success #f))
     (connect app command-line
              (lambda (app command-line)
                (let ((args (get-arguments command-line)))
                  (vector-for-each (lambda (i world)
                                     (format #t "Hello, ~a~%" world))
                                   args)
                  (quit app)
                  (set! success (vector= string=? args #("hello" "world" "darkness, my old friend")))
                  0)))
     (run app #("hello" "world" "darkness, my old friend"))
     success)))
