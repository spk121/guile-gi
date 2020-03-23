(define-module (doc ex-load-by-name)
  #:use-module (gi)
  #:use-module (gi repository))
(require "GLib" "2.0")
(load-by-name "GLib" "MainLoop")
(load-by-name "GLib" "MainContext")
(export <GMainLoop>
        main-loop:new
        is-running?
        <GMainContext>
        get-context)
