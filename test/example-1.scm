(use-modules (gi))

(typelib-load "Gio" "2.0")
(typelib-load "Gtk" "3.0")
(typelib-load "GLib" "2.0")

(define (print-hello widget data)
  (display "Hello World\n"))

(define (activate app user-data)
  (let ((window (cast (ApplicationWindow-new app) <GtkApplicationWindow>))
	    (button-box (cast (ButtonBox-new 0) <GtkButtonBox>))
	    (button (Button-new-with-label "Hello World")))
    (call-method window "set-title" "Window")
    (call-method window "set-default-size" 200 200)
    (call-method window "show-all")

    (call-method window "add" button-box)

    (signal-connect button "clicked" print-hello #f)
    (signal-connect button "clicked" (lambda x
				       (call-method window "destroy")) #f)
    (call-method button-box "add" button)
    (call-method window "show-all")))

(define (main)
  (let ((app (Application-new "org.gtk.example" 0)))
    (signal-connect app "activate" activate #f)
    (call-method app "run" (length (command-line)) (command-line))))

(main)
