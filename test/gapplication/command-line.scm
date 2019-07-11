(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-43))

(typelib-require ("Gio" "2.0"))

(automake-test
 (begin
   (let ((app (make-gobject (get-gtype <GApplication>)
                            `(("application-id" . "gi.guile.Example")
                              ("flags" . ,APPLICATION_HANDLES_COMMAND_LINE))))
         (success #f))
     (with-object app
       (connect! command-line
         (lambda (app command-line)
           (let ((args (with-object command-line (get-arguments))))
             (vector-for-each (lambda (world)
                                (format #t "Hello, ~a~%" world))
                              args)
             (with-object app (quit))
             (set! success (vector= string=? args #("world" "darkness, my old friend")))
             0)))
       (run #("hello" "world" "darkness, my old friend")))
     success)))
