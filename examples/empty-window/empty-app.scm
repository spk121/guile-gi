(define-module (empty-window empty-app)
  #:use-module (gi)
  #:use-module (gi repository)
  #:use-module (oop goops)
  #:use-module (empty-window empty-app-window)
  #:export(empty-app-new))

(eval-when (compile load eval)
  (require "Gio" "2.0")
  (require "Gtk" "3.0")
  (load-by-name "Gio" "Application" LOAD_SIGNALS)
  (load-by-name "Gio" "ApplicationFlags")
  (load-by-name "Gtk" "Application" LOAD_INFO_ONLY)
  (load-by-name "Gtk" "Window" LOAD_METHODS))

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
         (make <EmptyApp> #:application-id "org.gtk.exampleapp"
               #:flags (number->application-flags 4))))
    (connect app activate empty-app-activate)
    app))
