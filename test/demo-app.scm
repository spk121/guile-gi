(use-modules (ice-9 eval-string))

(define str (gi-load-repository "Gio" "2.0"))
(eval-string str)

(define example-app (application-new "com.lonelycactus.example-app"
				     APPLICATION_HANDLES_OPEN))

(application-run example-app (length (command-line)) (command-line))
