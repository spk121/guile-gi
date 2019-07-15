(define-module (empty-window empty-app)
  #:use-module (gi)
  #:use-module (gi gio-2)
  #:use-module (gi gtk-3)
  #:use-module (empty-window empty-app-window)
  #:export(empty-app-new))

(define <EmptyApp>
  (register-type
   "EmptyApp"                           ; type name
   <GtkApplication>                     ; parent_type
   #f                                   ; No additional properties
   #f))                                 ; No new signals

(define (empty-app-init app)
  #f)

(define (empty-app-activate app)
  (present (empty-app-window-new app)))

(define (empty-app-new)
  (let ((app
         (make <EmptyApp> #:application-id "org.gtk.exampleapp" #:flags 4)))
    (connect app (make-signal #:name "activate") empty-app-activate)
    app))
