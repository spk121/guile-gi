(use-modules (gi)
             (gi documentation)
             (gi repository)
             (srfi srfi-64))

(test-begin "typelib")

(define %test-symbol '<GMainLoop>)
(define %test-lib '("GLib" "2.0"))

(test-assert "typelib->module"
  (let* ((module (apply typelib->module '(test typelib->module) %test-lib))
         (iface (module-public-interface module)))
    (module-defined? iface %test-symbol)))

(test-assert "use-typelibs"
  (begin
    (save-module-excursion
     (lambda ()
       (eval
        `(begin
           (define-module (test use-typelibs)
             #:use-module (gi))

           (use-typelibs ,%test-lib))
        (interaction-environment))))

    (let* ((module (resolve-module '(test use-typelibs)))
           (iface (resolve-interface '(test use-typelibs))))
      (and (not (module-defined? iface %test-symbol))
           (module-defined? module %test-symbol)))))

(test-assert "documentation"
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

(test-end "typelib")
