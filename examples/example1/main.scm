(use-modules (gi)
             (gi gtk-3)
             (example1 exampleapp))

(define (main)
  (let ((app (example-app-new)))
    (send app (run (length (command-line)) (command-line)))))

(main)
