(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0") ("Gio" "2.0"))

(automake-test
 (begin
   (let ((app (make <GApplication>
                #:application-id "gi.guile.Example"))
         (result #f))
     (connect app activate
              (lambda (app)
                (set! (application:resource-base-path app)
                      "/gi/guile/resource/base_path")
                (set! result
                      (equal? (application:resource-base-path app)
                              "/gi/guile/resource/base_path"))
                (quit app)))
     (run app (command-line))
     result)))
