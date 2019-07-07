(use-modules (gi)
             (gi oop)
             (oop goops)
             (test automake-test-lib))

(typelib-require ("Gio" "2.0"))

(define application-id (make <property> #:nick "application-id"))

(automake-test
 (begin
   (let ((app (create <GApplication>
                (application-id "gi.guile.Example"))))
     (and (equal? (application-id app) "gi.guile.Example")
          (begin
            (set! (application-id app) "gi.guile.NewExample")
            (equal? (application-id app) "gi.guile.NewExample"))))))
