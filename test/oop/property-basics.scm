(use-modules (gi)
             (gi oop)
             (oop goops)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0") ("Gio" "2.0"))

(automake-test
 (begin
   (let* ((app (create <GApplication>
                 (application-id "gi.guile.Example")))
          (application-id ((@@ (gi oop) %object-get-pspec) app "application-id")))
     (and (equal? (application-id app) "gi.guile.Example")
          (begin
            (set! (application-id app) "gi.guile.NewExample")
            (equal? (application-id app) "gi.guile.NewExample"))))))
