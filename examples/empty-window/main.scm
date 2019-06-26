(use-modules (gi)
             (gi gtk-3)
             (empty-window empty-app))

(define (main)
  (let ((app (empty-app-new)))
    (send app (run (length (command-line)) (command-line)))))

(main)
