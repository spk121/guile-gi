(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0") ("Gio" "2.0"))

(automake-test
 (begin
   (let ((app (make-gobject <GApplication>
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
       (run (command-line)))
     result)))
