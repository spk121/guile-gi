(define-module (empty-window empty-app-window)
  #:use-module (gi)
  #:use-module (gi repository)
  #:export (empty-app-window-new))

(require "Gtk" "3.0")
(load-by-name "Gtk" "ApplicationWindow" LOAD_INFO_ONLY)

(define <EmptyAppWindow>
  (register-type
   "EmptyAppWindow"                     ; type name
   <GtkApplicationWindow>               ; parent_type
   #f                                   ; No additional properties
   #f))                                 ; No new signals

(define (empty-app-window-new app)
  (make <EmptyAppWindow> #:application app))
