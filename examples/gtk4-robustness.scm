;; Copyright (C) 2026 Michael L. Gran

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A robustness survey of guile-gi against a large introspected
;; library: by default GTK 4 and, recursively, everything it depends on
;; (Gdk, Gsk, Graphene, Pango, cairo, HarfBuzz, Gio, GObject, GLib, ...).
;;
;; For every namespace it
;;  1. loads every introspection info, one at a time, catching Scheme
;;     errors and collecting guile-gi's warnings and criticals;
;;  2. compares every function, method and constructor in the typelib
;;     with the procedures guile-gi actually created, and explains why
;;     each missing one was not bound;
;;  3. counts introspected features guile-gi does not handle at all
;;     (struct fields, virtual functions, deprecated infos, types
;;     without a GType);
;;  4. if a display is available, instantiates every concrete GObject
;;     class and reads every readable property through guile-gi's
;;     GValue marshalling.
;;
;; Each namespace and phase runs in a child process, so that a crash in
;; C is reported (with the item that caused it) instead of ending the
;; survey; the child is then restarted with that item skipped.
;;
;; The result is a report of the introspection functionality that
;; guile-gi does not yet handle.
;;
;; Usage, from the top of the build tree:
;;
;;   tools/uninstalled-env xvfb-run -a guile examples/gtk4-robustness.scm
;;
;; Options:
;;   --namespace NS VERSION  survey NS-VERSION instead of Gtk 4.0
;;   --no-instantiate        skip phase 4
;;   --examples N            examples to list per category (default 5)
;;   --records FILE          also write every raw record to FILE

;;; Code:

(use-modules ((gi) #:select ((make . gi:make)))
             ((gi repository) #:select (require infos load load-by-name))
             (gi types)
             (gi logging)
             ((gi girepository) #:prefix gir:)
             (oop goops)
             (sxml simple)
             ((sxml xpath) #:select (sxpath))
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 optargs)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex))

;;; Small helpers

(define (emit . record)
  ;; Children report to the parent with one s-expression per line.
  (write record)
  (newline)
  (force-output))

(define (split-ns-ver str)
  (let ((i (string-index str #\-)))
    (values (substring str 0 i) (substring str (1+ i)))))

(define (exception->string key args)
  (catch #t
    (lambda ()
      (match args
        ((subr (? string? message) (? list? fmt-args) . _)
         (apply simple-format #f message fmt-args))
        (_ (simple-format #f "~s" args))))
    (lambda _ (simple-format #f "~s" args))))

(define-syntax-rule (guarded item body ...)
  ;; Run BODY, reporting a Scheme error for ITEM instead of raising it.
  (catch #t
    (lambda () body ... #t)
    (lambda (key . args)
      (emit 'error item (symbol->string key) (exception->string key args))
      #f)))

(define (type-description ti)
  ;; A readable name for a GITypeInfo.
  (let ((tag (gir:type-info-get-tag ti)))
    (if (eq? tag 'interface)
        (let ((iface (gir:type-info-get-interface ti)))
          (string-append (symbol->string (gir:base-info-get-type iface)) " "
                         (gir:base-info-get-namespace iface) "."
                         (gir:base-info-get-name iface)))
        (symbol->string tag))))

;;; Child: logging

(define %interesting-debug
  ;; Debug messages from guile-gi's loader that explain skipped items.
  (make-regexp "not loading|could not create a gsubr|invalid argument map|Unrepresentable type|Unsupported irepository|Unhandled item type"))

(define (install-capturing-logger!)
  (install-custom-logger!
   (lambda* (#:key log-level message #:allow-other-keys)
     ;; GLib levels: 4 error, 8 critical, 16 warning, 128 debug
     (cond
      ((< log-level 128) (emit 'log log-level message))
      ((regexp-exec %interesting-debug message) (emit 'debug message))))))

;;; Child: phase 1, loading

(define (load-namespace ns ver skip report?)
  ;; Load every info of NS into a fresh module, like typelib->module, but
  ;; one at a time.  Returns the module.
  (require ns ver)
  (let* ((module (resolve-module (list 'robustness (string->symbol ns))))
         (_ (unless (module-public-interface module)
              ;; As typelib->module does: definitions are exported into it.
              (let ((interface (make-module)))
                (set-module-name! interface (module-name module))
                (set-module-kind! interface 'interface)
                (set-module-public-interface! module interface))))
         (all (infos ns))
         ;; infos returns the non-deprecated infos in typelib order.
         (names (filter-map (lambda (i)
                              (let ((info (gir:get-info ns i)))
                                (and (not (gir:base-info-is-deprecated? info))
                                     (gir:base-info-get-name info))))
                            (iota (gir:get-n-infos ns))))
         (names (if (= (length names) (length all))
                    names
                    (map (const "(unnamed)") all))))
    (save-module-excursion
     (lambda ()
       (set-current-module module)
       (for-each
        (lambda (info name)
          (let ((item (string-append ns "." name)))
            (unless (member item skip)
              (when report? (emit 'progress item))
              (let ((defs '()))
                (if report?
                    (guarded item (set! defs (load info)))
                    (catch #t (lambda () (set! defs (load info))) (const #f)))
                (module-export! module defs)))))
        all names)))
    module))

;;; Child: phase 2, coverage

(define (documented-procedures info)
  ;; ((container fname long-name) ...) for the procedures %document
  ;; describes for INFO; %document uses the same argument map as the
  ;; binding code, so these are the names guile-gi binds them under.
  (let* ((xml (with-output-to-string
                (lambda () ((@@ (gi documentation) %document) info))))
         (tree (xml->sxml (string-append "<r xmlns:c=\"urn:c\">" xml "</r>")
                          #:namespaces '((c . "urn:c"))))
         (attr (lambda (node n)
                 (let ((v ((sxpath `(@ ,n *text*)) node))) (and (pair? v) (car v)))))
         (entry (lambda (container fn)
                  (map (lambda (proc)
                         (list container (attr fn 'name)
                               (or (attr proc 'long-name) (attr proc 'name))))
                       ((sxpath '(scheme procedure)) fn)))))
    (append
     (append-map (cut entry #f <>) ((sxpath '(r function)) tree))
     (append-map (lambda (c)
                   ;; Constructors and static functions are <function>s.
                   (append-map (cut entry (attr c 'name) <>)
                               (append ((sxpath '(method)) c) ((sxpath '(function)) c))))
                 ((sxpath '(r *)) tree)))))

(define (bound? module name)
  (let ((g (module-ref module (string->symbol name) #f)))
    (and (is-a? g <generic>)
         (any (lambda (m)
                (eq? (procedure-name (method-procedure m)) (string->symbol name)))
              (generic-function-methods g)))))

(define (no-gtype? info)
  (memv (gir:registered-type-info-get-g-type info) '(0 4))) ; INVALID, NONE

(define (type-problem ti depth)
  ;; Why guile-gi can't represent this type, or #f.
  (let ((tag (gir:type-info-get-tag ti)))
    (cond
     ((> depth 4) #f)
     ((eq? tag 'interface)
      (let* ((iface (gir:type-info-get-interface ti))
             (kind (gir:base-info-get-type iface))
             (name (string-append (gir:base-info-get-namespace iface) "."
                                  (gir:base-info-get-name iface))))
        (case kind
          ((unresolved) (string-append "unresolved type " name))
          ((struct union object interface boxed)
           (and (no-gtype? iface)
                (string-append (symbol->string kind) " without a GType: " name)))
          ((callback)
           (let ((inner (callable-problem iface (1+ depth))))
             (and inner (string-append "callback " name " with " inner))))
          (else #f))))
     ((memq tag '(array glist gslist ghash))
      (any (lambda (i) (type-problem (gir:type-info-get-param-type ti i) (1+ depth)))
           (iota (if (eq? tag 'ghash) 2 1))))
     (else #f))))

(define (callable-problem fn depth)
  (or (type-problem (gir:callable-info-get-return-type fn) depth)
      (any (lambda (i)
             (type-problem (gir:arg-info-get-type (gir:callable-info-get-arg fn i)) depth))
           (iota (gir:callable-info-get-n-args fn)))))

(define (callables-of info)
  ;; All functions, methods and constructors in a top-level GI info.
  (let ((methods (lambda (n get) (map (cut get info <>) (iota (n info))))))
    (case (gir:base-info-get-type info)
      ((function) (list info))
      ((object) (methods gir:object-info-get-n-methods gir:object-info-get-method))
      ((struct) (methods gir:struct-info-get-n-methods gir:struct-info-get-method))
      ((union) (methods gir:union-info-get-n-methods gir:union-info-get-method))
      ((interface) (methods gir:interface-info-get-n-methods gir:interface-info-get-method))
      ((enum flags) (methods gir:enum-info-get-n-methods gir:enum-info-get-method))
      (else '()))))

(define (coverage ns module skipped-reasons skip)
  (let ((documented (make-hash-table))
        (total 0) (bound 0))
    (for-each (lambda (info i)
                (let ((item (simple-format #f "~a documentation of info ~a" ns i)))
                  (unless (member item skip)
                    (emit 'progress item)
                    (for-each (match-lambda
                                ((container fname long-name)
                                 (hash-set! documented (cons container fname) long-name)))
                              (or (false-if-exception (documented-procedures info)) '())))))
              (infos ns)
              (iota (length (infos ns))))
    (for-each
     (lambda (i)
       (let* ((info (gir:get-info ns i))
              (container (and (not (eq? (gir:base-info-get-type info) 'function))
                              (gir:base-info-get-name info))))
         (for-each
          (lambda (fn)
           (let ((item (string-append ns "." (or container "") ":"
                                      (gir:base-info-get-name fn))))
            (unless (member item skip)
            (emit 'progress item)
            (set! total (1+ total))
            (let* ((fname (gir:base-info-get-name fn))
                   (c-name (or (gir:function-info-get-symbol fn) fname))
                   (long-name (hash-ref documented (cons container fname))))
              (cond
               ((and long-name (bound? module long-name))
                (set! bound (1+ bound)))
               (else
                (emit 'unbound ns c-name
                      (cond
                       ((gir:base-info-is-deprecated? info)
                        "in a deprecated type, which guile-gi skips")
                       ((and container
                             (memq (gir:base-info-get-type info) '(struct union object interface))
                             (no-gtype? info))
                        (string-append "method of a type without a GType: " ns "." container))
                       ((and long-name (hash-ref skipped-reasons fname)))
                       ((and container (hash-ref skipped-reasons container))
                        => (lambda (why) (string-append "its type was not loaded: " why)))
                       ((callable-problem fn 0))
                       (long-name "documented but not bound")
                       (else "not representable (no argument map)")))))))))
          (callables-of info))))
     (iota (gir:get-n-infos ns)))
    (emit 'coverage ns bound total)))

(define (unsupported-features ns)
  ;; Introspected features guile-gi has no support for at all.
  (let ((fields '()) (vfuncs '()) (deprecated '()) (no-gtype '()))
    (for-each
     (lambda (i)
       (let* ((info (gir:get-info ns i))
              (name (string-append ns "." (gir:base-info-get-name info)))
              (kind (gir:base-info-get-type info)))
         (when (gir:base-info-is-deprecated? info)
           (set! deprecated (cons name deprecated)))
         (when (and (memq kind '(struct union)) (no-gtype? info))
           (set! no-gtype (cons name no-gtype)))
         (let ((n-fields (case kind
                           ((struct) (gir:struct-info-get-n-fields info))
                           ((union) (gir:union-info-get-n-fields info))
                           ((object) (gir:object-info-get-n-fields info))
                           (else 0)))
               (n-vfuncs (case kind
                           ((object) (gir:object-info-get-n-vfuncs info))
                           ((interface) (gir:interface-info-get-n-vfuncs info))
                           (else 0))))
           (when (> n-fields 0) (set! fields (cons (cons name n-fields) fields)))
           (when (> n-vfuncs 0) (set! vfuncs (cons (cons name n-vfuncs) vfuncs))))))
     (iota (gir:get-n-infos ns)))
    (emit 'unsupported ns "struct, union and class fields (no accessors)"
          (apply + (map cdr fields)) (map car fields))
    (emit 'unsupported ns "virtual functions (cannot be overridden or called)"
          (apply + (map cdr vfuncs)) (map car vfuncs))
    (emit 'unsupported ns "deprecated top-level infos (skipped when loading)"
          (length deprecated) deprecated)
    (emit 'unsupported ns "structs and unions without a GType (not loaded)"
          (length no-gtype) no-gtype)))

;;; Child: phase 4, instantiation

(define (display-available?)
  (catch #t
    (lambda ()
      (gir:require "Gtk" #:version "4.0")
      (let ((m (resolve-module '(robustness gtk-init))))
        (save-module-excursion
         (lambda ()
           (set-current-module m)
           (load-by-name "Gtk" "init_check")))
        ((module-ref m 'init-check?))))
    (const #f)))

(define (gobject-class? info)
  (and (eq? (gir:base-info-get-type info) 'object)
       (not (gir:object-info-get-abstract? info))
       (not (gir:object-info-get-fundamental? info))
       (not (no-gtype? info))))

(define %keep-alive
  ;; Objects created by instantiate-all are kept until the child exits:
  ;; otherwise the GC may finalize one during a later item, and a crash
  ;; in its finalizer would be blamed on that item.
  '())

(define (instantiate-all ns module skip)
  (let ((created 0) (read 0))
    (for-each
     (lambda (i)
       (let ((info (gir:get-info ns i)))
         (when (and (gobject-class? info) (not (gir:base-info-is-deprecated? info)))
           (let* ((name (string-append ns "." (gir:base-info-get-name info)))
                  (class (gtype-get-scheme-type (gir:registered-type-info-get-g-type info))))
             (unless (or (member name skip) (not (is-a? class <class>)))
               (emit 'progress name)
               (let ((obj #f))
                 (when (guarded name (set! obj (gi:make class)))
                   (set! %keep-alive (cons obj %keep-alive))
                   (set! created (1+ created))
                   (for-each
                    (lambda (j)
                      (let* ((prop (gir:object-info-get-property info j))
                             (pname (gir:base-info-get-name prop))
                             (item (string-append name ":" pname)))
                        (when (and (memq 'readable (gir:property-info-get-flags prop))
                                   (not (member item skip)))
                          (emit 'progress item)
                          (when (guarded (string-append item " ("
                                                        (type-description
                                                         (gir:property-info-get-type prop))
                                                        ")")
                                  ((@@ (gi oop) %get-property)
                                   obj ((@@ (gi oop) %object-get-pspec) obj pname)))
                            (set! read (1+ read))))))
                    (iota (gir:object-info-get-n-properties info))))))))))
     (iota (gir:get-n-infos ns)))
    (emit 'instantiated ns created read)))

;;; Child: entry point

(define (run-child phase ns ver skip)
  (install-capturing-logger!)
  (gir:require ns #:version ver)
  (case phase
    ((load)
     (let ((skipped-reasons (make-hash-table)))
       ;; Remember the loader's reasons for not binding a function.
       (install-custom-logger!
        (lambda* (#:key log-level message #:allow-other-keys)
          (cond
           ((< log-level 128) (emit 'log log-level message))
           ((regexp-exec %interesting-debug message)
            (emit 'debug message)
            (let ((m (string-match "^([^ ]+) - not loading [a-z ]+ because (.*)$" message)))
              (when m
                (hash-set! skipped-reasons (match:substring m 1) (match:substring m 2))))))))
       (let ((module (load-namespace ns ver skip #t)))
         (coverage ns module skipped-reasons skip)
         (unsupported-features ns))))
    ((instantiate)
     (if (display-available?)
         (let ((module (begin
                         (install-custom-logger! (lambda* (#:key #:allow-other-keys) #f))
                         (load-namespace ns ver '() #f))))
           (install-capturing-logger!)
           (instantiate-all ns module skip))
         (emit 'no-display))))
  (emit 'done))

;;; Parent

(define %guile
  (or (getenv "GUILE")
      (false-if-exception (readlink "/proc/self/exe"))
      "guile"))

(define %script (car (command-line)))

(define (run-phase phase ns ver)
  ;; Run PHASE for NS-VER in children until one finishes, skipping each
  ;; item that crashed one.  Returns (records . crashes).
  (setenv "GUILE_LOAD_PATH" (string-join %load-path ":"))
  (setenv "GUILE_LOAD_COMPILED_PATH" (string-join %load-compiled-path ":"))
  (let loop ((skip '()) (crashes '()) (attempt 0))
    (let* ((port (apply open-pipe* OPEN_READ %guile "--no-auto-compile" %script
                        "--child" (symbol->string phase) ns ver skip))
           ;; Read line by line and keep only records: libraries may
           ;; print to stdout too, e.g. GLib's "Bail out!" before an abort.
           (records (let read-all ((acc '()))
                      (let ((line (read-line port)))
                        (if (eof-object? line)
                            (reverse acc)
                            (let ((r (catch #t
                                       (lambda () (call-with-input-string line read))
                                       (const #f))))
                              (read-all (if (and (pair? r) (symbol? (car r)))
                                            (cons r acc)
                                            acc)))))))
           (status (close-pipe port))
           (last-item (any (match-lambda (('progress item) item) (_ #f)) (reverse records))))
      (cond
       ((member '(done) records)
        (cons records (reverse crashes)))
       ;; Give up if the crash can't be attributed to a new item.
       ((or (not last-item) (member last-item skip) (> attempt 50))
        (cons records
              (reverse (cons (list phase ns (or last-item "(startup)") (describe-status status))
                             crashes))))
       (else
        (format (current-error-port) "  ~a ~a: crashed at ~a (~a); retrying without it~%"
                phase ns last-item (describe-status status))
        (loop (cons last-item skip)
              (cons (list phase ns last-item (describe-status status)) crashes)
              (1+ attempt)))))))

(define (describe-status status)
  (cond
   ((not status) "unknown")
   ((status:term-sig status) => (lambda (sig) (simple-format #f "signal ~a" sig)))
   ((status:exit-val status) => (lambda (v) (simple-format #f "exit ~a" v)))
   (else "unknown")))

(define (normalize message)
  ;; Group messages that differ only in names and numbers: drop a
  ;; leading "name - ", and replace quoted strings, <classes>,
  ;; Namespace.Names and free-standing numbers with "_".
  (fold (lambda (rx msg) (regexp-substitute/global #f rx msg 'pre "_" 'post))
        (regexp-substitute #f (or (string-match "^[^ ]+ - " message)
                                  (string-match "^" message))
                           'post)
        (map make-regexp
             '("#<[^>]*>" "<[A-Za-z0-9_]+>" "'[^']*'" "\"[^\"]*\""
               "[A-Za-z][A-Za-z0-9_]*\\.[A-Za-z0-9_]+" "\\b[0-9]+\\b"))))

(define (group-by key-fn items)
  ;; ((key count example ...) ...), largest group first.
  (let ((table (make-hash-table)))
    (for-each (lambda (item)
                (hash-set! table (key-fn item)
                           (cons item (hash-ref table (key-fn item) '()))))
              items)
    (sort (hash-map->list (lambda (k v) (list k (length v) (reverse v))) table)
          (lambda (a b) (> (cadr a) (cadr b))))))

(define (print-groups title groups n-examples show)
  (format #t "~%~a~%" title)
  (if (null? groups)
      (format #t "  none~%")
      (for-each (match-lambda
                  ((key count items)
                   (format #t "  ~a  ~a~%" count key)
                   (for-each (lambda (item) (format #t "        ~a~%" (show item)))
                             (take items (min n-examples (length items))))))
                groups)))

(define (report namespaces results crashes n-examples)
  (define (records-of tag) (filter (lambda (r) (eq? (car r) tag)) results))
  (format #t "guile-gi robustness survey~%")
  (format #t "Namespaces: ~a~%" (string-join namespaces " "))

  (format #t "~%Binding coverage (functions, methods and constructors)~%")
  (let ((totals (records-of 'coverage)))
    (for-each (match-lambda
                ((_ ns bound total)
                 (format #t "  ~a ~a/~a bound~%" ns bound total)))
              totals)
    (let ((b (apply + (map caddr totals))) (t (apply + (map cadddr totals))))
      (format #t "  total ~a/~a bound, ~a not bound~%" b t (- t b))))

  (print-groups "Why functions were not bound"
                (group-by (match-lambda ((_ ns c-name reason) (normalize reason)))
                          (records-of 'unbound))
                n-examples
                (match-lambda ((_ ns c-name reason) (string-append c-name ": " reason))))

  (format #t "~%Introspection features guile-gi does not handle~%")
  (for-each (match-lambda
              ((kind . recs)
               (let ((count (apply + (map cadddr recs)))
                     (examples (append-map (lambda (r) (list-ref r 4)) recs)))
                 (format #t "  ~a  ~a~%" count kind)
                 (for-each (lambda (e) (format #t "        ~a~%" e))
                           (take examples (min n-examples (length examples)))))))
            (map (lambda (kind) (cons kind (filter (lambda (r) (equal? (caddr r) kind))
                                                   (records-of 'unsupported))))
                 (delete-duplicates (map caddr (records-of 'unsupported)))))

  ;; Every crash is listed.  Crashes creating an object are often the
  ;; library's own preconditions (e.g. a required construct property);
  ;; crashes reading a property go through guile-gi's GValue code.
  (print-groups "Crashes (C-level; each was skipped and the survey continued)"
                (group-by (match-lambda
                            ((phase ns item status)
                             (simple-format #f "~a, ~a"
                                            (cond ((eq? phase 'load) "loading")
                                                  ((string-index item #\:) "reading a property")
                                                  (else "creating an object"))
                                            status)))
                          crashes)
                (length crashes)
                (match-lambda ((phase ns item status) item)))

  (print-groups "Scheme errors"
                (group-by (match-lambda ((_ item key message)
                                         (string-append key ": " (normalize message))))
                          (records-of 'error))
                n-examples
                (match-lambda ((_ item key message) (string-append item ": " message))))

  (print-groups "Items guile-gi's loader skipped, by its own explanation"
                (group-by (match-lambda ((_ message) (normalize message)))
                          (delete-duplicates (records-of 'debug)))
                n-examples
                (match-lambda ((_ message) message)))

  (print-groups "Warnings and criticals from guile-gi"
                (group-by (match-lambda ((_ level message) (normalize message)))
                          (delete-duplicates (records-of 'log)))
                n-examples
                (match-lambda ((_ level message) message)))

  (format #t "~%Instantiation~%")
  (cond
   ((member '(no-display) results)
    (format #t "  skipped: no display (run under xvfb-run, for example)~%"))
   ((null? (records-of 'instantiated))
    (format #t "  skipped~%"))
   (else
    (for-each (match-lambda
                ((_ ns created read)
                 (format #t "  ~a: ~a objects created, ~a properties read~%" ns created read)))
              (records-of 'instantiated)))))

(define (parse-options args)
  ;; Later settings are prepended, so assq-ref finds them first.
  (let loop ((args args)
             (opts (list (list 'namespace "Gtk" "4.0") (cons 'instantiate #t)
                         (cons 'examples 5) (cons 'records #f))))
    (match args
      (() opts)
      (("--namespace" ns ver . rest) (loop rest (acons 'namespace (list ns ver) opts)))
      (("--no-instantiate" . rest) (loop rest (acons 'instantiate #f opts)))
      (("--examples" n . rest) (loop rest (acons 'examples (string->number n) opts)))
      (("--records" file . rest) (loop rest (acons 'records file opts)))
      ((other . rest)
       (format (current-error-port) "unknown option ~a~%" other)
       (exit 2)))))

(define (main args)
  (match args
    (("--child" phase ns ver . skip)
     (run-child (string->symbol phase) ns ver skip))
    (_
     (let* ((opts (parse-options args))
            (top (assq-ref opts 'namespace))
            (_ (gir:require (car top) #:version (cadr top)))
            (namespaces (append (gir:get-dependencies (car top))
                                (list (string-append (car top) "-" (cadr top)))))
            (results '())
            (crashes '()))
       (for-each
        (lambda (ns-ver)
          (call-with-values (lambda () (split-ns-ver ns-ver))
            (lambda (ns ver)
              (format (current-error-port) "surveying ~a~%" ns-ver)
              (for-each
               (lambda (phase)
                 (match (run-phase phase ns ver)
                   ((records . new-crashes)
                    (set! results (append results records))
                    (set! crashes (append crashes new-crashes)))))
               (if (assq-ref opts 'instantiate) '(load instantiate) '(load))))))
        namespaces)
       (report namespaces results crashes (assq-ref opts 'examples))
       (let ((file (assq-ref opts 'records)))
         (when file
           (with-output-to-file file
             (lambda ()
               (for-each (lambda (r) (write r) (newline)) results)
               (for-each (lambda (c) (write (cons 'crash c)) (newline)) crashes)))))))))

(main (cdr (command-line)))
