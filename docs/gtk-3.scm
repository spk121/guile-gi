(define-module (gi gtk-3)
  #:use-module (gi)
  #:use-module (gi repository))

(typelib->module (current-module) "Gtk" "3.0")
