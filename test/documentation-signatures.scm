(use-modules (gi) (gi repository) (oop goops) (sxml simple) (sxml xpath)
             (srfi srfi-1) (srfi srfi-26) (srfi srfi-64) (ice-9 match)
             ((gi girepository) #:prefix gir:))

;; The documentation of each function's arguments and return values
;; (from %document) must match the procedure src/func actually builds:
;; its arities, including optional arguments, and its return values.

(define %document (@@ (gi documentation) %document))

;; Documented signatures, from %document's XML: one entry per
;; procedure, with the GI names of its container (or #f) and function.
(define (documented info)
  (let* ((xml (with-output-to-string (lambda () (%document info))))
         (tree (xml->sxml (string-append "<r xmlns:c=\"urn:c\">" xml "</r>")
                          #:namespaces '((c . "urn:c"))))
         (attr (lambda (node n) (let ((v ((sxpath `(@ ,n *text*)) node))) (and (pair? v) (car v)))))
         (entry (lambda (container fn)
                  (map (lambda (proc)
                         (list container (attr fn 'name)
                               (or (attr proc 'long-name) (attr proc 'name))
                               ((sxpath '(argument @ name *text*)) proc)
                               ((sxpath '(argument @ optional *text*)) proc)
                               ((sxpath '(return @ name *text*)) proc)))
                       ((sxpath '(scheme procedure)) fn)))))
    (append
     (append-map (cut entry #f <>) ((sxpath '(r function)) tree))
     (append-map (lambda (c)
                   (append-map (cut entry (attr c 'name) <>)
                               ((sxpath '(method)) c)))
                 ((sxpath '(r *)) tree)))))

;; Real arities: methods whose gsubr carries the procedure's name.
(define (real-arities module name)
  (let ((g (module-ref module (string->symbol name) #f)))
    (if (is-a? g <generic>)
        (sort (delete-duplicates
               (filter-map (lambda (m)
                             (and (equal? (procedure-name (method-procedure m)) (string->symbol name))
                                  (length (method-specializers m))))
                           (generic-function-methods g)))
              <)
        '())))

;; Expected Scheme return values, rebuilt from the introspection data the way
;; the argument map does: the return value unless void, then each out/inout
;; argument that is not the length of a C array.
(define (expected-returns fn)
  (let* ((n (gir:callable-info-get-n-args fn))
         (args (map (cut gir:callable-info-get-arg fn <>) (iota n)))
         (ret (gir:callable-info-get-return-type fn))
         (len-index (lambda (ti)
                      (and (eq? (gir:type-info-get-tag ti) 'array)
                           (eq? (gir:type-info-get-array-type ti) 'c)
                           (let ((i (gir:type-info-get-array-length ti))) (and (>= i 0) i)))))
         (children (filter-map len-index (cons ret (map gir:arg-info-get-type args)))))
    (append
     (if (and (eq? (gir:type-info-get-tag ret) 'void) (not (gir:type-info-is-pointer? ret))) '() '("%return"))
     (filter-map (lambda (a i)
                   (and (memq (gir:arg-info-get-direction a) '(out inout))
                        (not (memv i children))
                        (gir:base-info-get-name a)))
                 args (iota n)))))

(define (mismatches ns ver)
  (gir:require ns #:version ver)
  (let ((module (typelib->module (list 'test 'documentation-signatures (string->symbol ns))
                                 ns ver))
        (found '()))
    (define (gir-function container fname)
      (if container
          (let ((c (gir:find-by-name ns container)))
            (and c (case (gir:base-info-get-type c)
                     ((object) (gir:object-info-find-method c fname))
                     ((struct) (gir:struct-info-find-method c fname))
                     ((union) (gir:union-info-find-method c fname))
                     ((interface) (gir:interface-info-find-method c fname))
                     (else #f))))
          (gir:find-by-name ns fname)))
    (define (dashes s) (string-map (lambda (c) (if (char=? c #\_) #\- c)) s))
    (define (check-one doc)
      (match doc
        ((container fname name args optional returns)
         (let ((arities (real-arities module name))
               (fn (gir-function container fname)))
           ;; Functions that aren't bound have nothing to compare.
           (unless (or (null? arities) (not fn))
             (let* ((n (length args))
                    (documented-arities (iota (1+ (length optional)) (- n (length optional)))))
               (unless (equal? arities documented-arities)
                 (set! found (cons (list name 'arities documented-arities 'real arities) found))))
             (let ((expected (map dashes (expected-returns fn)))
                   (documented (map dashes returns)))
               (unless (equal? expected documented)
                 (set! found (cons (list name 'returns documented 'expected expected) found)))))))))
    (for-each (lambda (info) (for-each check-one (documented info))) (infos ns))
    (reverse found)))

(test-begin "documentation-signatures")

(test-equal "GLib" '() (mismatches "GLib" "2.0"))
(test-equal "GObject" '() (mismatches "GObject" "2.0"))

(if (false-if-exception (gir:require "Gio" #:version "2.0"))
    (test-equal "Gio" '() (mismatches "Gio" "2.0"))
    (begin
      (test-skip 1)
      (test-assert "Gio" #f)))

(test-end "documentation-signatures")
