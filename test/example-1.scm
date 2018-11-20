(use-modules (gi)
	     (Gtk)
	     ((Gio) #:prefix Gio)
	     )

(load-typelib "Gtk" "3.0")
(load-typelib "Gio" "2.0")

(define (print-hello widget data)
  (display "Hello World\n"))

(define (activate app user-data)
  (let ((window (ApplicationWindow-new app))
	(button-box (ButtonBox-new 0))
	(button (Button-new-with-label "Hello World")))
    (Window-set-title window "Window")
    (Window-set-default-size window 200 200)
    (Widget-show-all window)

    (Container-add window button-box)

    (signal-connect button "clicked" print-hello #f)
    (signal-connect button "clicked" (lambda x
				       (Widget-destroy window)) #f)
    (Container-add button-box button)
    (Widget-show-all window)))

(define (main)
  (let ((app (Application-new "org.gtk.example" 0)))
    (signal-connect app "activate" activate #f)
    (GioApplication-run app (length (command-line)) (command-line))))

(main)
