(use-modules (gi)
             (test automake-test-lib))

(automake-test
 (begin
   (typelib-load "Gio" "2.0")
   (let ((app (make-gobject (get-gtype <GApplication>)
                            '(("application-id" . "gi.guile.Example"))))
         (result #f))
     (connect app (activate
                   (lambda (app user-data)
                     (gobject-set-property! app "resource-base-path"
                                            "/gi/guile/resource/base_path")
                     (set! result
                           (equal? (gobject-get-property app "resource-base-path")
                                   "/gi/guile/resource/base_path"))
                     (send app (quit)))))
     (send app (run (length (command-line)) (command-line)))
     result)))
