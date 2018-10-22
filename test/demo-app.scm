(use-modules (ice-9 eval-string))

(define str (gi-load-repository "Gio" "2.0"))
(define str2 (gi-load-repository "Gtk" "3.0"))

;; (display str2)  
(eval-string str)
(eval-string str2)

(define example-app (application-new
		     "com.lonelycactus.example-app"
		     APPLICATION_HANDLES_OPEN))

(define win #f)
(define (example-app-activate app extra)
  (format #t "In example-app-activate: app = ~s extra = ~s" app extra)
  (newline)
  (set! win (application-window-new app))
  (format #t "application-window-new returns ~S" win)
  (newline)
  (window-present win))

(signal-connect example-app "activate" example-app-activate)

(application-run example-app
		 (1- (length (command-line)))
		 (list->vector (command-line)))
