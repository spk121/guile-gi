(use-modules (gi)
             (test automake-test-lib))

(save-module-excursion
 (lambda ()
   (eval
    '(begin
       (define-module (test use-typelibs example)
         #:use-module (gi))

       (use-typelibs ("Gio" "2.0")))
    (interaction-environment))))

(automake-test
 (let ((module-spec '(test use-typelibs example))
       (var 'GApplication?))
   (let ((module (resolve-module module-spec))
         (interface (resolve-interface module-spec)))
     (and (not (module-defined? interface var))
          (module-defined? module var)))))
