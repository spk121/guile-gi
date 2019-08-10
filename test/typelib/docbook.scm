(use-modules (gi documentation)
             (test automake-test-lib))

;; this file is for experimentation purposes
;; it does not actually add value as a testcase

(automake-test
 (let ((%typelib (typelib "GObject" "2.0"))
       (%gir (false-if-exception (gir "GObject" "2.0")))
       (%doc '()))
   (set! %doc (parse %typelib %doc))
   (close %typelib)
   (when %gir
     (set! %doc (parse %gir %doc))
     (close %gir))
   (->docbook %doc)
   #t))
