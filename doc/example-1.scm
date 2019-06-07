(use-modules (gi)
             (gi gio-2)
             (gi gtk-3)
             (gi glib-2))
(define (print-hello widget data)
  (display "Hello World\n"))

(define (activate-callback app user-data)
  (let ((window (cast (ApplicationWindow-new app) <GtkApplicationWindow>))
        (button-box (cast (ButtonBox-new 0) <GtkButtonBox>))
        (button (Button-new-with-label "Hello World")))
    (send window (set-title "Window"))
    (send window (set-default-size 200 200))
    (send window (show-all))
    (send window (add button-box))

    (connect button (clicked print-hello #f))
    (connect button (clicked (lambda x
                               (send window (destroy)))
                             #f))
    (send button-box (add button))
    (send window (show-all))))

(define (main)
  (let ((app (Application-new "org.gtk.example" 0)))
    (connect app (activate activate-callback #f))
    (send app (run (length (command-line)) (command-line)))))

(main)
