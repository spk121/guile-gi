(use-modules (gi)
	     (gi gtk-3)
	     (gi gio-2)
	     )

(define (activate app user-data)
  (let ((window (ApplicationWindow-new app)))
    (call-method window "set-title" "Window")
    (call-method window set-default-size 200 200)
    (call-method window "show-all")))

(define (main)
  (let ((app (Application-new "org.gtk.example" 0)))
    (signal-connect app "activate" activate #f)
    (call-method app "run" (length (command-line)) (command-line))))

(main)
