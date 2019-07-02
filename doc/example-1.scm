(use-modules (gi)
             (gi gio-2)
             (gi gtk-3)
             (gi glib-2))

(define (print-hello widget)
  (display "Hello World\n"))

(define (activate-callback app)
  (let* ((window (create <GtkApplicationWindow>
                   (application app)
                   (default-height 200)
                   (default-width 200)
                   (title "Window")))
         (button-box (create <GtkButtonBox> (parent window)))
         (button (create <GtkButton>
                   (parent button-box)
                   (label "Hello world"))))
    (modify-signals button
      (connect clicked print-hello)
      (connect clicked (lambda _ (with-object window (destroy)))))

    (with-object window (show-all))))

(define (main)
  (let ((app (create <GtkApplication> (application-id "org.gtk.example"))))
    (modify-signals app (connect activate activate-callback))
    (with-object app (run (length (command-line)) (command-line)))))

(main)
