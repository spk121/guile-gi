(use-modules (gi)
             (test automake-test-lib))

(automake-test
 (let ((app-id "gi.guile.Example"))
   (typelib-load "Gio" "2.0")
   (equal?
    (with-object (create <GApplication>
                   (application-id ((lambda () app-id))))
      (get-application-id)))))
