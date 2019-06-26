(define-module (example1 exampleapp)
  #:use-module (gi)
  #:use-module (gi gio-2)
  #:use-module (gi gtk-3)
  #:use-module (example1 exampleappwindow)
  #:export(example-app-new))

(define <ExampleApp>
  (register-type
   "ExampleApp"                         ; type name
   <GtkApplication>                     ; parent_type
   #f                                   ; No additional properties
   #f                                   ; No new signals
   #f))                                 ; No disposer func

;; (define EXAMPLE_APP_TYPE (get-gtype <ExampleApp>))

(define (example-app-init app)
  #f)

(define (example-app-activate app dummy)
  (let ((win (example-app-window-new app)))
    (send win (present))))

(define (example-app-open app files n_files hint)
  (let ((windows (send app (get-windows))))
    (let ((win
           (if window
               (windows->data)
               (example-app-window-new app))))
      (for-each files
                (send win (open)))
      (send win (present)))))


(define (example-app-new)
  (let ((app
         (make-gobject
          ;; GType of GType integer
          <ExampleApp>
          ;; Alist of properties
          '(("application-id" . "org.gtk.exampleapp")
            ("flags" . 4)))))
    (connect app (activate example-app-activate))
    (connect app (open example-app-open))
    app))
