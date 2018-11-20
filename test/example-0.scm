(use-modules (gi)
	     (Gtk)
	     ((Gio) #:prefix Gio)
	     )

(load-typelib "Gtk" "3.0")
(load-typelib "Gio" "2.0")

(define (activate app user-data)
  (let ((window (ApplicationWindow-new app)))
    (Window-set-title window "Window")
    (Window-set-default-size window 200 200)
    (Widget-show-all window)))

(define (main)
  (let ((app (Application-new "org.gtk.example" 0)))
    (signal-connect app "activate" activate #f)
    (GioApplication-run app (length (command-line)) (command-line))))

(main)
