(use-modules (gi)
             (gi documentation)
             (gi repository)
             (srfi srfi-64))

(use-modules (oop goops) (ice-9 peg) (sxml simple) (sxml xpath) (srfi srfi-1))

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

(test-assert "documentation headings nest validly in DocBook"
  ;; Every Markdown heading used to become a refsect2, which DocBook
  ;; doesn't allow inside a method's refsect2 or inside a table cell.
  (let* ((gir "<repository xmlns=\"http://www.gtk.org/introspection/core/1.0\">
<namespace name=\"GObject\"><class name=\"Object\">
<doc>The base object.\n\n# Overview heading\n\nOverview text.</doc>
<method name=\"notify\"><doc>Emits notify.\n\n## Details heading\n\nDetail text.</doc>
<parameters><parameter name=\"property_name\"><doc>The name.\n\n## Note heading\n\nNote text.</doc>
</parameter></parameters></method></class></namespace></repository>")
         (%typelib (typelib "GObject" "2.0"))
         (%doc (parse gir (parse %typelib '())))
         (tree (xml->sxml (with-output-to-string (lambda () (->docbook %doc)))))
         (titles (lambda (path) ((sxpath path) tree))))
    (close %typelib)
    (and (member "Overview heading" (titles '(// refsect1 refsect2 title *text*)))
         (member "Details heading" (titles '(// refsect2 refsect3 title *text*)))
         (member "Note heading" (titles '(// td para emphasis *text*)))
         (null? ((sxpath '(// td // refsect2)) tree))
         (null? ((sxpath '(// refsect2 refsect2)) tree)))))

(define (documented-procedure lib type long-name)
  ;; The <procedure> element %document emits for LONG-NAME.
  (let* ((xml (with-output-to-string
                (lambda () ((@@ (gi documentation) %document) (info lib type)))))
         (tree (xml->sxml (string-append "<r xmlns:c=\"urn:c\">" xml "</r>")
                          #:namespaces '((c . "urn:c")))))
    (or (find (lambda (proc)
                (equal? ((sxpath '(@ long-name *text*)) proc) (list long-name)))
              ((sxpath '(// procedure)) tree))
        (error "procedure is not documented" long-name))))

(test-equal "documented returns omit array-length outputs"
  ;; g_bytes_get_data's size output only sizes the returned array; the
  ;; procedure returns just the data, so the documentation must too.
  '("%return")
  ((sxpath '(return @ name *text*))
   (documented-procedure "GLib" "Bytes" "bytes:get-data")))

(test-equal "documented optional arguments"
  ;; The trailing nullable argument may be omitted.  (This needs a type
  ;; with a GType in every supported GLib: e.g. GBookmarkFile only has
  ;; one since GLib 2.76, and without one its methods are neither bound
  ;; nor documented.)
  '(("self" ()) ("key" ()) ("data" ("1")))
  (map (lambda (arg)
         (list (car ((sxpath '(@ name *text*)) arg))
               ((sxpath '(@ optional *text*)) arg)))
       ((sxpath '(argument))
        (documented-procedure "GObject" "Object" "object:set-data"))))

(test-assert "typelib->module without a version"
  ;; The version defaulted to #f, which require rejects.
  (module? (typelib->module '(test typelib->module no-version) "GLib")))

;; GValueArray has been deprecated since GLib 2.32.

(test-assert "infos leaves out what is deprecated"
  (begin
    (require "GObject" "2.0")
    (let ((current (length (infos "GObject")))
          (all (length (infos "GObject" #t)))
          (only (length (infos "GObject" 'only))))
      (and (positive? only) (= all (+ current only))))))

(test-equal "typelib->module leaves out what is deprecated"
  '(#t #f)
  (let ((module (typelib->module '(test typelib->module current) "GObject" "2.0")))
    (list (module-defined? module '<GBinding>)
          (module-defined? module '<GValueArray>))))

(test-equal "typelib->module #:deprecated?"
  '(#t #t)
  (let ((module (typelib->module '(test typelib->module deprecated) "GObject" "2.0"
                                 #:deprecated? #t)))
    (list (module-defined? module '<GBinding>)
          (module-defined? module '<GValueArray>))))

(test-assert "typelib->module #:deprecated? without a version"
  (module-defined? (typelib->module '(test typelib->module deprecated no-version) "GObject"
                                    #:deprecated? #t)
                   '<GValueArray>))

(test-equal "typelib->module #:deprecated? adds to a module"
  '(#f #t)
  (let* ((module (typelib->module '(test typelib->module added) "GObject" "2.0"))
         (before (module-defined? module '<GValueArray>)))
    (typelib->module module "GObject" "2.0" #:deprecated? #t)
    (list before (module-defined? (module-public-interface module) '<GValueArray>))))

(test-equal "use-typelibs #:deprecated?"
  '(#f #t #t)
  (begin
    (save-module-excursion
     (lambda ()
       (eval
        '(begin
           (define-module (test use-typelibs current)
             #:use-module (gi))
           (use-typelibs ("GObject" "2.0"))
           (define before (defined? '<GValueArray>)))
        (interaction-environment))
       ;; Separately, since the modules of typelibs are shared and
       ;; use-typelibs loads them when it is expanded.
       (eval
        '(begin
           (define-module (test use-typelibs deprecated)
             #:use-module (gi))
           ;; The other options still go to use-modules.
           (use-typelibs (("GObject" "2.0") #:deprecated? #t #:prefix gobject/)))
        (interaction-environment))))
    (let ((deprecated (resolve-module '(test use-typelibs deprecated))))
      (list (module-ref (resolve-module '(test use-typelibs current)) 'before)
            (module-defined? deprecated 'gobject/<GValueArray>)
            (module-defined? deprecated 'gobject/<GBinding>)))))

;; The documentation describes what the same option would bind.
(test-equal "documentation leaves out what is deprecated, unless asked"
  '(#f #t #t #t)
  (let ((documents?
         (lambda (name . options)
           (let* ((port (apply typelib "GObject" "2.0" options))
                  (text (with-output-to-string
                          (lambda () (->docbook (parse port '()))))))
             (close port)
             (and (string-contains text name) #t)))))
    (list (documents? "value-array:append")
          (documents? "value-array:append" #:deprecated? #t)
          (documents? "binding:unbind")
          (documents? "binding:unbind" #:deprecated? #t))))

;; Short names that are syntax in Guile would take that syntax away from
;; whoever imports the typelib.  GSettings has a method called delay and
;; GDBusObjectManagerServer one called export.
(if (false-if-exception (require "Gio" "2.0"))
    (let* ((module (typelib->module '(test typelib->module syntax) "Gio" "2.0"))
           (interface (module-public-interface module)))
      (test-equal "methods named after syntax have no short name"
        '(#f #f)
        (map (lambda (name) (module-defined? interface name)) '(delay export)))

      (test-equal "methods named after syntax keep their long names"
        '(#t #t #t)
        (map (lambda (name) (module-defined? interface name))
             '(settings:delay
               dbus-object-manager-server:export
               dbus-interface-skeleton:export?)))

      (test-assert "other methods keep their short names"
        (module-defined? interface 'revert))

      (test-assert "load stays a generic function with the methods of typelibs"
        ;; (gi repository) makes load generic itself.
        (and (module-defined? interface 'loadable-icon:load)
             (is-a? (module-ref interface 'load) <generic>)))

      (test-equal "the syntax still works where the typelib is used"
        '(2 #t)
        (begin
          (save-module-excursion
           (lambda ()
             (eval
              '(begin
                 (define-module (test use-typelibs syntax)
                   #:use-module (gi))
                 (use-typelibs ("Gio" "2.0"))
                 (define results (list (begin 1 2) (promise? (delay 3)))))
              (interaction-environment))))
          (module-ref (resolve-module '(test use-typelibs syntax)) 'results)))

      (test-equal "documentation gives them under their long names"
        '(#t #f)
        (let* ((port (typelib "Gio" "2.0"))
               (text (with-output-to-string
                       (lambda () (->docbook (parse port '()))))))
          (close port)
          ;; A method is listed under its short name if it has one.
          (list (and (string-contains text "<title>settings:delay</title>") #t)
                (and (string-contains text "<title>delay</title>") #t)))))
    (test-skip 6))

(test-end "typelib")
