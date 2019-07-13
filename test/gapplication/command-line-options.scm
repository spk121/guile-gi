(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-43))

(typelib-require ("GLib" "2.0") ("GObject" "2.0") ("Gio" "2.0"))

(automake-test
 (begin
   (let ((app (make-gobject <GApplication>
                            `(("application-id" . "gi.guile.Example")
                              ("flags" . ,APPLICATION_HANDLES_COMMAND_LINE))))
         (success #f))
     (add-main-option app "hello" (char->integer #\h) 0
                      OPTION_ARG_STRING_ARRAY "" #f)

     (connect app (make-signal #:name "command-line")
              (lambda (app command-line)
                (let* ((dict (get-options-dict command-line))
                       (hello (lookup-value dict "hello" #f))
                       (args (variant:get-strv hello)))
                  (vector-for-each (lambda (i arg) (format #t "Hello, ~a~%" arg))
                                   args)
                  (set! success (vector= string=? args #("world" "darkness, my old friend"))))
                (quit app)
                0))
     (application:run app #("command-line" "-h" "world" "-h" "darkness, my old friend"))
     success)))
