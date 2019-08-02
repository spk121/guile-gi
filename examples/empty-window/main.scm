(use-modules (gi) (gi repository)
             (empty-window empty-app))

(require "Gio" "2.0")
(load-by-name "Gio" "Application" LOAD_METHODS)

(define (main)
  (let ((app (empty-app-new)))
    (run app (command-line))))

(main)
