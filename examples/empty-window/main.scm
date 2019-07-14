(use-modules (gi)
             (empty-window empty-app))

(use-typelibs ("Gtk" "3.0"))

(define (main)
  (let ((app (empty-app-new)))
    (run app (command-line))))

(main)
