(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Gio" "2.0"))

(automake-test
 (let ((app-id "gi.guile.Example"))
   (equal?
    (with-object (create <GApplication>
                   (application-id ((lambda () app-id))))
      (get-application-id)))))
