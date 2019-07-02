(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-43))

(automake-test
 (begin
   (typelib-load "Gio" "2.0")
   (let ((app (make-gobject (get-gtype <GApplication>)
                            `(("application-id" . "gi.guile.Example")
                              ("flags" . ,APPLICATION_HANDLES_COMMAND_LINE))))
         (success #f))
     (modify-signals app
       (connect command-line
         (lambda (app command-line)
           (let ((args (with-object command-line (get-arguments))))
             (vector-for-each (lambda (world)
                                (format #t "Hello, ~a~%" world))
                              args)
             (with-object app (quit))
             (set! success (vector= string=? args #("world" "darkness, my old friend")))
             0))))
     (with-object app (run 3 #("hello" "world" "darkness, my old friend")))
     success)))
