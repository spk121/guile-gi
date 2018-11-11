(use-modules (gi)
	     (Gtk)
	     (Gio)
	     )

(load-typelib "Gtk" "3.0")
(load-typelib "Gio" "2.0")

(define (print-hello widget data)
  (display "Hello World\n"))

(define (activate app user-data)
  (let ((window (GtkApplicationWindow-new app))
	(button-box (GtkButtonBox-new 0))
	(button (GtkButton-new-with-label "Hello World")))
    (GtkWindow-set-title window "Window")
    (GtkWindow-set-default-size window 200 200)
    (GtkWidget-show-all window)

    (GtkContainer-add window button-box)

    (signal-connect button "clicked" print-hello #f)
    (signal-connect button "clicked" (lambda x
				       (GtkWidget-destroy window)) #f)
    (GtkContainer-add button-box button)
    (GtkWidget-show-all window)))

(define (main)
  (let ((app (GtkApplication-new "org.gtk.example" 0)))
    (signal-connect app "activate" activate #f)
    (GioApplication-run app (length (command-line)) (command-line))))

(main)
