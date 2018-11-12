(use-modules (gi)
	     (Gtk)
	     (Gio)
	     )

(load-typelib "Gtk" "3.0")
(load-typelib "Gio" "2.0")

(define (activate app user-data)
  (let ((window (GtkApplicationWindow-new app)))
    (GtkWindow-set-title window "Window")
    (GtkWindow-set-default-size window 200 200)
    (GtkWidget-show-all window)))

(define (main)
  (let ((app (GtkApplication-new "org.gtk.example" 0)))
    (signal-connect app "activate" activate #f)
    (GioApplication-run app (length (command-line)) (command-line))))

(main)
