(use-modules (gi)
             (gi gtk-3)
             (empty-window empty-app))

(define (main)
  (let ((app (empty-app-new)))
    (with-object app (run (length (command-line)) (command-line)))))

(main)
