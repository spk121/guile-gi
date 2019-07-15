(use-modules (gi) (oop goops))

(use-typelibs ("GLib" "2.0")
              ("GObject" "2.0")
              ("Gio" "2.0")
              ("Gtk" "3.0"))

(define (print-hello widget)
  (display "Hello World\n"))

(define-method (connect obj (signal <symbol>) (handler <procedure>))
  (connect obj (make <signal> #:name (symbol->string signal)) handler))

(define (activate-callback app)
  (let* ((window (make <GtkApplicationWindow>
                   #:application app
                   #:default-height 200
                   #:default-width 200
                   #:title "Window"))
         (button-box (make <GtkButtonBox> #:parent window))
         (button (make <GtkButton>
                   #:parent button-box
                   #:label "Hello world")))
    (connect button 'clicked print-hello)
    (connect button 'clicked (lambda _ (destroy window)))
    (show-all window)))

(define (main)
  (let ((app (make <GtkApplication> #:application-id "org.gtk.example")))
    (connect app 'activate activate-callback)
    (run app (command-line))))

(main)
