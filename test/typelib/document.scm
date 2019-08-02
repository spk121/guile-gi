(use-modules (gi documentation)
             (test automake-test-lib))

(automake-test
 (let ((doc (typelib "GLib" "2.0")))
   (->guile-procedures.txt doc)
   #t))
