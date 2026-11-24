;;; Reading GIR files, for tests that are driven by what a library's
;;; introspection data says rather than by what guile-gi made of it.
;;;
;;; The bindings are made from typelibs.  A test that also took its list
;;; of functions from the typelib, through guile-gi, could not notice a
;;; function that guile-gi fails to bind, or binds under the wrong name.
;;; These procedures read the .gir XML that the typelib was compiled
;;; from instead, using nothing of guile-gi.

(define-module (test gir)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (sxml simple)
  #:use-module ((sxml xpath) #:select (sxpath))
  #:export (find-gir
            gir-callables

            callable?
            callable-kind callable-container callable-name
            callable-parameters callable-return
            callable-scheme-name
            callable-mentions-type?

            value?
            value-name value-type value-c-type value-direction
            value-caller-allocates? value-array? value-transfer

            find-binding
            strip-suffix))

;; The namespaces of GIR, as the prefixes that xml->sxml gives the names
;; of elements and attributes.
(define %namespaces
  '((core . "http://www.gtk.org/introspection/core/1.0")
    (c . "http://www.gtk.org/introspection/c/1.0")
    (glib . "http://www.gtk.org/introspection/glib/1.0")))

;; The .gir files of the test libraries are built next to their
;; typelibs.
(define (find-gir namespace version)
  "Return the name of the GIR file of NAMESPACE, or #f if there is none
in the directories of GI_TYPELIB_PATH."
  (let ((file (string-append namespace "-" version ".gir"))
        (separator (if (string-contains %host-type "mingw") #\; #\:)))
    (any (lambda (directory)
           (let ((name (string-append directory "/" file)))
             (and (file-exists? name) name)))
         (string-split (or (getenv "GI_TYPELIB_PATH") "") separator))))

;; A function, method or constructor.
(define-record-type <callable>
  (make-callable kind container name parameters return node)
  callable?
  (kind callable-kind)                  ; function, method or constructor
  (container callable-container)        ; "Object", or #f at the top level
  (name callable-name)                  ; "array_fixed_int_in"
  (parameters callable-parameters)      ; a list of <value>, without the instance
  (return callable-return)              ; a <value>
  (node callable-node))

;; A parameter or a return value.
(define-record-type <value>
  (make-value name type c-type direction caller-allocates? array? transfer)
  value?
  (name value-name)
  (type value-type)                     ; "gint", "utf8", "none", "GLib.List", ...
  (c-type value-c-type)                 ; "gint*", when the GIR has it
  (direction value-direction)           ; in, out or inout
  (caller-allocates? value-caller-allocates?)
  (array? value-array?)
  (transfer value-transfer))            ; "none", "container" or "full"

(define (attribute node name)
  (match ((sxpath `(@ ,name *text*)) node)
    ((text . _) text)
    (() #f)))

(define (child node name)
  (match ((sxpath (list name)) node)
    ((first . _) first)
    (() #f)))

(define (node->value node)
  (let* ((array (child node 'core:array))
         (type (or array (child node 'core:type))))
    (make-value (attribute node 'name)
                (cond (array (or (attribute array 'name) "array"))
                      (type (attribute type 'name))
                      (else #f))
                (and type (attribute type 'c:type))
                (string->symbol (or (attribute node 'direction) "in"))
                (equal? (attribute node 'caller-allocates) "1")
                (and array #t)
                (attribute node 'transfer-ownership))))

(define (node->callable kind container node)
  (make-callable kind container (attribute node 'name)
                 (map node->value
                      ((sxpath '(core:parameters core:parameter)) node))
                 (node->value (child node 'core:return-value))
                 node))

;; Bindings can't call what the library says is not introspectable, and
;; a function that another one shadows gives up its name.
(define (usable? node)
  (not (or (equal? (attribute node 'introspectable) "0")
           (attribute node 'shadowed-by)
           (attribute node 'moved-to))))

(define (gir-callables file)
  "Return the functions, methods and constructors that the GIR file FILE
describes, as a list of <callable>."
  (let* ((document (call-with-input-file file
                     (lambda (port)
                       (xml->sxml port #:namespaces %namespaces
                                  #:trim-whitespace? #t))))
         (namespace (child (child document 'core:repository) 'core:namespace))
         (callables
          (lambda (container node)
            (append-map
             (match-lambda
               ((kind . element)
                (map (lambda (node) (node->callable kind container node))
                     (filter usable? ((sxpath (list element)) node)))))
             '((function . core:function)
               (method . core:method)
               (constructor . core:constructor))))))
    (append
     (callables #f namespace)
     (append-map
      (lambda (element)
        (append-map (lambda (node) (callables (attribute node 'name) node))
                    ((sxpath (list element)) namespace)))
      '(core:class core:record core:union core:interface)))))

;; The naming convention of the bindings, as the manual gives it: names
;; are lower case, underscores become dashes, and a dash separates a
;; lower case letter from a capital that follows it.
(define (gir-name->scheme-name name)
  (let loop ((chars (string->list name))
             (after-lower? #f)
             (result '()))
    (match chars
      (() (list->string (reverse result)))
      ((c . rest)
       (cond
        ((char-lower-case? c) (loop rest #t (cons c result)))
        ((char-upper-case? c)
         (loop rest #f
               (cons (char-downcase c)
                     (if after-lower? (cons #\- result) result))))
        ((char=? c #\_) (loop rest #f (cons #\- result)))
        (else (loop rest #f (cons c result))))))))

(define (callable-scheme-name callable)
  "Return the name guile-gi gives CALLABLE, without the ? or ! that it
may add to it."
  (let ((name (gir-name->scheme-name (callable-name callable))))
    (if (callable-container callable)
        (string-append (gir-name->scheme-name (callable-container callable))
                       ":" name)
        name)))

(define (callable-mentions-type? callable types)
  "Return #t if a parameter or the return value of CALLABLE is an array,
or has one of the GIR type names TYPES, at any depth."
  (let ((node (callable-node callable)))
    (or (pair? ((sxpath '(// core:array)) node))
        (any (lambda (type) (member (attribute type 'name) types))
             ((sxpath '(// core:type)) node))
        #f)))

(define (strip-suffix name)
  "Return the string NAME without the ? and ! that guile-gi appends to
the names of predicates and of procedures that modify their arguments."
  (string-trim-right name (char-set #\? #\!)))

(define (find-binding module callable)
  "Return the name, a symbol, under which CALLABLE is bound in MODULE,
or #f if it isn't bound there."
  (let ((base (callable-scheme-name callable)))
    (any (lambda (suffix)
           (let ((name (string->symbol (string-append base suffix))))
             (and (module-variable module name) name)))
         '("" "?" "!" "!?" "?!"))))
