(use-modules (gi)
             (gi documentation)
             (gi repository)
             (srfi srfi-64))

(use-modules (ice-9 peg))

(define %markdown-sample
  "Some text.\n\n# A heading\n\nMore text.\n\n```txt\n# a comment\n[group]\n```\n\nThe end.\n")

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
    ;; ->docbook was not exercised at all before.
    (string-prefix? "<?xml" (with-output-to-string (lambda () (->docbook %doc))))))

(test-equal "documentation markdown parses headings and fenced code"
  ;; A level-1 heading or a fenced block with '#' lines used to end the
  ;; parse, silently dropping the rest of the docstring.
  (string-length %markdown-sample)
  (peg:end (match-pattern (@@ (gi documentation) doc-string) %markdown-sample)))

(test-assert "typelib->module without a version"
  ;; The version defaulted to #f, which require rejects.
  (module? (typelib->module '(test typelib->module no-version) "GLib")))

(test-end "typelib")
