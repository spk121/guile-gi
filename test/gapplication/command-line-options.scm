(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-43))

(automake-test
 (begin
   (use-typelibs ("GLib" "2.0") ("Gio" "2.0"))
   (let ((app (make-gobject (get-gtype <GApplication>)
                            `(("application-id" . "gi.guile.Example")
                              ("flags" . ,APPLICATION_HANDLES_COMMAND_LINE))))
         (success #f))
     (send app (add-main-option "hello" (char->integer #\h) 0
                                OPTION_ARG_STRING_ARRAY "" #f))

     (connect app (command-line
                   (lambda (app command-line data)
                     (let* ((dict (send command-line (get-options-dict)))
                            (hello (send dict (lookup-value "hello" #f)))
                            (args (send hello (get-strv))))
                       (vector-for-each (lambda (i arg) (format #t "Hello, ~a~%" arg))
                                        args)
                       (set! success (vector= string=? args #("world" "darkness, my old friend"))))
                     (send app (quit))
                     0)))
     (send app (run 5 #("command-line" "-h" "world" "-h" "darkness, my old friend")))
     success)))
