(use-modules (gi)
	     (Gtk))

(define (activate app user-data)
  (let ((window (ApplicationWindow-new app)))
    (Window-set-title window "Window")
    (Window-set-default-size window 200 200)
    (Widget-show-all window)))

(define (main)
  (let ((app (Application-new "org.gtk.example" 0)))
    (signal-connect app "activate" activate #f)
    (Application-run app argc argv)))
