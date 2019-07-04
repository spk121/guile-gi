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
     (with-object app (add-main-option "hello" (char->integer #\h) 0
                                       OPTION_ARG_STRING_ARRAY "" #f))

     (with-object app
       (connect! command-line
         (lambda (app command-line)
           (let* ((dict (with-object command-line (get-options-dict)))
                  (hello (with-object dict (lookup-value "hello" #f)))
                  (args (with-object hello (get-strv))))
             (vector-for-each (lambda (i arg) (format #t "Hello, ~a~%" arg))
                              args)
             (set! success (vector= string=? args #("world" "darkness, my old friend"))))
           (send app (quit))
           0)))
     (with-object app (run 5 #("command-line" "-h" "world" "-h" "darkness, my old friend")))
     success)))
