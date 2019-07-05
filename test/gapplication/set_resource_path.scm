(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Gio" "2.0"))

(automake-test
 (begin
   (let ((app (make-gobject (get-gtype <GApplication>)
                            '(("application-id" . "gi.guile.Example"))))
         (result #f))
     (with-object app
       (connect! activate
         (lambda (app)
           (gobject-set-property! app "resource-base-path"
                                  "/gi/guile/resource/base_path")
           (set! result
                 (equal? (gobject-get-property app "resource-base-path")
                         "/gi/guile/resource/base_path"))
           (with-object app (quit))))
       (run (length (command-line)) (command-line)))
     result)))
