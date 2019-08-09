(use-modules (gi documentation)
             (test automake-test-lib))

(automake-test
 (let ((%typelib (typelib "GObject" "2.0"))
       (%gir (false-if-exception (gir "GObject" "2.0")))
       (%doc '()))
   (set! %doc (parse %typelib %doc))
   (close %typelib)
   (when %gir
     (set! %doc (parse %gir %doc))
     (close %gir))
   (->guile-procedures.txt %doc)
   #t))
