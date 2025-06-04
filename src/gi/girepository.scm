;; -*- scheme -*-
;; Copyright (C) 2025 Michael L. Gran
;; Modernized GIRepository Scheme interface

(define-module (gi repository)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:export (
    ;; Repository management
    require-namespace
    require-namespace-private
    get-loaded-namespaces
    get-dependencies
    get-immediate-dependencies
    
    ;; Info lookup
    find-by-name
    find-by-gtype
    find-by-error-domain
    get-info
    get-all-infos
    get-infos-by-type
    
    ;; Base info operations
    gi-name
    gi-namespace
    gi-type
    gi-deprecated?
    gi-equal?
    gi-attributes
    gi-container
    
    ;; Type predicates
    gi-base-info?
    gi-callable-info?
    gi-function-info?
    gi-object-info?
    gi-interface-info?
    gi-struct-info?
    gi-enum-info?
    gi-union-info?
    gi-constant-info?
    gi-field-info?
    gi-property-info?
    gi-arg-info?
    gi-type-info?
    
    ;; Configuration
    auto-load-dependencies!
    clear-cache!
    repository-stats
    preload-namespace
    
    ;; Enhanced utilities
    namespace-loaded?
    info-type->string
    type-tag->string
    
    ;; Constants
    GI-MAJOR-VERSION
    GI-MINOR-VERSION
    GI-MICRO-VERSION
    
    ;; Type tags
    GI-TYPE-TAG-VOID
    GI-TYPE-TAG-BOOLEAN
    GI-TYPE-TAG-INT8
    GI-TYPE-TAG-UINT8
    GI-TYPE-TAG-INT16
    GI-TYPE-TAG-UINT16
    GI-TYPE-TAG-INT32
    GI-TYPE-TAG-UINT32
    GI-TYPE-TAG-INT64
    GI-TYPE-TAG-UINT64
    GI-TYPE-TAG-FLOAT
    GI-TYPE-TAG-DOUBLE
    GI-TYPE-TAG-GTYPE
    GI-TYPE-TAG-UTF8
    GI-TYPE-TAG-FILENAME
    GI-TYPE-TAG-ARRAY
    GI-TYPE-TAG-INTERFACE
    GI-TYPE-TAG-GLIST
    GI-TYPE-TAG-GSLIST
    GI-TYPE-TAG-GHASH
    GI-TYPE-TAG-ERROR
    GI-TYPE-TAG-UNICHAR
    
    ;; Info types
    GI-INFO-TYPE-INVALID
    GI-INFO-TYPE-FUNCTION
    GI-INFO-TYPE-CALLBACK
    GI-INFO-TYPE-STRUCT
    GI-INFO-TYPE-BOXED
    GI-INFO-TYPE-ENUM
    GI-INFO-TYPE-FLAGS
    GI-INFO-TYPE-OBJECT
    GI-INFO-TYPE-INTERFACE
    GI-INFO-TYPE-CONSTANT
    GI-INFO-TYPE-UNION
    GI-INFO-TYPE-VALUE
    GI-INFO-TYPE-SIGNAL
    GI-INFO-TYPE-VFUNC
    GI-INFO-TYPE-PROPERTY
    GI-INFO-TYPE-FIELD
    GI-INFO-TYPE-ARG
    GI-INFO-TYPE-TYPE
    GI-INFO-TYPE-UNRESOLVED
    
    ;; Function flags
    GI-FUNCTION-IS-METHOD
    GI-FUNCTION-IS-CONSTRUCTOR
    GI-FUNCTION-IS-GETTER
    GI-FUNCTION-IS-SETTER
    GI-FUNCTION-WRAPS-VFUNC
    GI-FUNCTION-THROWS
    
    ;; Transfer modes
    GI-TRANSFER-NOTHING
    GI-TRANSFER-CONTAINER
    GI-TRANSFER-EVERYTHING
    
    ;; Directions
    GI-DIRECTION-IN
    GI-DIRECTION-OUT
    GI-DIRECTION-INOUT
    
    ;; Load flags
    GI-IREPOSITORY-LOAD-FLAG-LAZY
  ))

;; Load the C extension
(eval-when (eval load compile)
  (dynamic-call "gig_init_girepository" (dynamic-link)))

;; ============================================================================
;; Repository Management
;; ============================================================================

(define* (require-namespace namespace #:optional version #:key (lazy? #f))
  "Load a GObject Introspection namespace.
   Returns #t if successful, throws an error if not."
  (let ((flags (if lazy? GI-IREPOSITORY-LOAD-FLAG-LAZY 0)))
    (irepository-require namespace (or version #f) flags)))

(define (require-namespace-private typelib-dir namespace version #:key (lazy? #f))
  "Load a private typelib from a specific directory."
  (let ((flags (if lazy? GI-IREPOSITORY-LOAD-FLAG-LAZY 0)))
    (irepository-require-private typelib-dir namespace version flags)))

(define (get-loaded-namespaces)
  "Get a list of all loaded namespaces."
  (irepository-get-loaded-namespaces))

(define (get-dependencies namespace)
  "Get all dependencies of a namespace (recursive)."
  (irepository-get-dependencies namespace))

(define (get-immediate-dependencies namespace)
  "Get immediate dependencies of a namespace (non-recursive)."
  (irepository-get-immediate-dependencies namespace))

;; ============================================================================
;; Info Lookup
;; ============================================================================

(define (find-by-name namespace name)
  "Find an info by namespace and name."
  (irepository-find-by-name namespace name))

(define (find-by-gtype gtype)
  "Find an info by GType."
  (irepository-find-by-gtype gtype))

(define (find-by-error-domain error-domain)
  "Find an info by error domain."
  (irepository-find-by-error-domain error-domain))

(define (get-info namespace index)
  "Get info by namespace and index."
  (irepository-get-info namespace index))

(define (get-all-infos namespace)
  "Get all infos in a namespace."
  (irepository-get-all-infos namespace))

(define (get-infos-by-type namespace info-type)
  "Get all infos of a specific type in a namespace."
  (filter (lambda (info)
            (= (gi-type info) info-type))
          (get-all-infos namespace)))

;; ============================================================================
;; Base Info Operations
;; ============================================================================

(define (gi-name info)
  "Get the name of a GI info object."
  (gi-base-info-get-name info))

(define (gi-namespace info)
  "Get the namespace of a GI info object."
  (gi-base-info-get-namespace info))

(define (gi-type info)
  "Get the type of a GI info object."
  (gi-base-info-get-type info))

(define (gi-deprecated? info)
  "Check if a GI info object is deprecated."
  (gi-base-info-is-deprecated? info))

(define (gi-equal? info1 info2)
  "Check if two GI info objects are equal."
  (gi-base-info-equal? info1 info2))

(define (gi-attributes info)
  "Get all attributes of a GI info object as an alist."
  (gi-base-info-get-attributes info))

(define (gi-container info)
  "Get the container of a GI info object."
  (gi-base-info-get-container info))

;; ============================================================================
;; Type Predicates
;; ============================================================================

(define (gi-base-info? obj)
  "Check if object is a GI base info."
  (gi-base-info? obj))

(define (gi-callable-info? info)
  "Check if info is a callable (function, method, etc.)."
  (let ((type (gi-type info)))
    (or (= type GI-INFO-TYPE-FUNCTION)
        (= type GI-INFO-TYPE-CALLBACK))))

(define (gi-function-info? info)
  "Check if info is a function."
  (= (gi-type info) GI-INFO-TYPE-FUNCTION))

(define (gi-object-info? info)
  "Check if info is an object."
  (= (gi-type info) GI-INFO-TYPE-OBJECT))

(define (gi-interface-info? info)
  "Check if info is an interface."
  (= (gi-type info) GI-INFO-TYPE-INTERFACE))

(define (gi-struct-info? info)
  "Check if info is a struct or boxed type."
  (let ((type (gi-type info)))
    (or (= type GI-INFO-TYPE-STRUCT)
        (= type GI-INFO-TYPE-BOXED))))

(define (gi-enum-info? info)
  "Check if info is an enum or flags type."
  (let ((type (gi-type info)))
    (or (= type GI-INFO-TYPE-ENUM)
        (= type GI-INFO-TYPE-FLAGS))))

(define (gi-union-info? info)
  "Check if info is a union."
  (= (gi-type info) GI-INFO-TYPE-UNION))

(define (gi-constant-info? info)
  "Check if info is a constant."
  (= (gi-type info) GI-INFO-TYPE-CONSTANT))

(define (gi-field-info? info)
  "Check if info is a field."
  (= (gi-type info) GI-INFO-TYPE-FIELD))

(define (gi-property-info? info)
  "Check if info is a property."
  (= (gi-type info) GI-INFO-TYPE-PROPERTY))

(define (gi-arg-info? info)
  "Check if info is an argument."
  (= (gi-type info) GI-INFO-TYPE-ARG))

(define (gi-type-info? info)
  "Check if info is a type info."
  (= (gi-type info) GI-INFO-TYPE-TYPE))

;; ============================================================================
;; Configuration
;; ============================================================================

(define (auto-load-dependencies! enable?)
  "Enable or disable automatic loading of dependencies."
  (gi-repository-set-auto-load-dependencies! enable?))

(define (clear-cache!)
  "Clear the repository cache."
  (gi-repository-clear-cache!))

(define (repository-stats)
  "Get repository statistics as an alist."
  (let ((stats (gi-repository-get-stats)))
    `((cached-types . ,(car stats))
      (memory-used . ,(cadr stats)))))

(define (preload-namespace namespace)
  "Preload all infos in a namespace for better performance."
  (gi-irepository-preload-namespace namespace))

;; ============================================================================
;; Enhanced Utilities
;; ============================================================================

(define (namespace-loaded? namespace)
  "Check if a namespace is loaded."
  (member namespace (get-loaded-namespaces)))

(define info-type-names
  `((,GI-INFO-TYPE-INVALID . "invalid")
    (,GI-INFO-TYPE-FUNCTION . "function")
    (,GI-INFO-TYPE-CALLBACK . "callback")
    (,GI-INFO-TYPE-STRUCT . "struct")
    (,GI-INFO-TYPE-BOXED . "boxed")
    (,GI-INFO-TYPE-ENUM . "enum")
    (,GI-INFO-TYPE-FLAGS . "flags")
    (,GI-INFO-TYPE-OBJECT . "object")
    (,GI-INFO-TYPE-INTERFACE . "interface")
    (,GI-INFO-TYPE-CONSTANT . "constant")
    (,GI-INFO-TYPE-UNION . "union")
    (,GI-INFO-TYPE-VALUE . "value")
    (,GI-INFO-TYPE-SIGNAL . "signal")
    (,GI-INFO-TYPE-VFUNC . "vfunc")
    (,GI-INFO-TYPE-PROPERTY . "property")
    (,GI-INFO-TYPE-FIELD . "field")
    (,GI-INFO-TYPE-ARG . "arg")
    (,GI-INFO-TYPE-TYPE . "type")
    (,GI-INFO-TYPE-UNRESOLVED . "unresolved")))

(define (info-type->string info-type)
  "Convert an info type constant to a string."
  (or (assq-ref info-type-names info-type)
      (format #f "unknown-~a" info-type)))

(define type-tag-names
  `((,GI-TYPE-TAG-VOID . "void")
    (,GI-TYPE-TAG-BOOLEAN . "boolean")
    (,GI-TYPE-TAG-INT8 . "int8")
    (,GI-TYPE-TAG-UINT8 . "uint8")
    (,GI-TYPE-TAG-INT16 . "int16")
    (,GI-TYPE-TAG-UINT16 . "uint16")
    (,GI-TYPE-TAG-INT32 . "int32")
    (,GI-TYPE-TAG-UINT32 . "uint32")
    (,GI-TYPE-TAG-INT64 . "int64")
    (,GI-TYPE-TAG-UINT64 . "uint64")
    (,GI-TYPE-TAG-FLOAT . "float")
    (,GI-TYPE-TAG-DOUBLE . "double")
    (,GI-TYPE-TAG-GTYPE . "gtype")
    (,GI-TYPE-TAG-UTF8 . "utf8")
    (,GI-TYPE-TAG-FILENAME . "filename")
    (,GI-TYPE-TAG-ARRAY . "array")
    (,GI-TYPE-TAG-INTERFACE . "interface")
    (,GI-TYPE-TAG-GLIST . "glist")
    (,GI-TYPE-TAG-GSLIST . "gslist")
    (,GI-TYPE-TAG-GHASH . "ghash")
    (,GI-TYPE-TAG-ERROR . "error")
    (,GI-TYPE-TAG-UNICHAR . "unichar")))

(define (type-tag->string type-tag)
  "Convert a type tag constant to a string."
  (or (assq-ref type-tag-names type-tag)
      (format #f "unknown-~a" type-tag)))

;; ============================================================================
;; High-level Convenience Functions
;; ============================================================================

(define* (with-namespace namespace proc #:key (version #f) (lazy? #f))
  "Execute proc with a namespace loaded, ensuring it's available."
  (unless (namespace-loaded? namespace)
    (require-namespace namespace version #:lazy? lazy?))
  (proc))

(define (find-all-by-name name #:optional (namespaces (get-loaded-namespaces)))
  "Find all infos with a given name across multiple namespaces."
  (filter-map (lambda (ns)
                (let ((info (find-by-name ns name)))
                  (and info (not (eq? info #f)) info)))
              namespaces))

(define (list-namespace-contents namespace #:key (type-filter #f))
  "List all contents of a namespace, optionally filtered by type."
  (let ((infos (get-all-infos namespace)))
    (if type-filter
        (filter (lambda (info) (= (gi-type info) type-filter)) infos)
        infos)))

(define (namespace-summary namespace)
  "Get a summary of a namespace's contents."
  (let ((infos (get-all-infos namespace)))
    (fold (lambda (info acc)
            (let* ((type (gi-type info))
                   (type-name (info-type->string type))
                   (count (or (assq-ref acc type-name) 0)))
              (assq-set! acc type-name (1+ count))))
          '()
          infos)))

(define (info-summary info)
  "Get a summary of an info object."
  `((name . ,(gi-name info))
    (namespace . ,(gi-namespace info))
    (type . ,(info-type->string (gi-type info)))
    (deprecated? . ,(gi-deprecated? info))
    (attributes . ,(gi-attributes info))))

(define (search-by-pattern pattern #:optional (namespaces (get-loaded-namespaces)))
  "Search for infos whose names match a pattern across namespaces."
  (let ((regex (make-regexp pattern)))
    (append-map (lambda (ns)
                  (filter (lambda (info)
                            (regexp-exec regex (gi-name info)))
                          (get-all-infos ns)))
                namespaces)))

;; ============================================================================
;; Pretty Printing
;; ============================================================================

(define (display-info info #:optional (port (current-output-port)))
  "Display detailed information about a GI info object."
  (format port "~a ~a:~a~%"
          (info-type->string (gi-type info))
          (gi-namespace info)
          (gi-name info))
  (when (gi-deprecated? info)
    (format port "  [DEPRECATED]~%"))
  (let ((attrs (gi-attributes info)))
    (unless (null? attrs)
      (format port "  Attributes:~%")
      (for-each (lambda (attr)
                  (format port "    ~a = ~a~%" (car attr) (cdr attr)))
                attrs))))

(define (display-namespace-summary namespace #:optional (port (current-output-port)))
  "Display a summary of a namespace's contents."
  (format port "Namespace: ~a~%" namespace)
  (for-each (lambda (entry)
              (format port "  ~a: ~a~%" (car entry) (cdr entry)))
            (namespace-summary namespace)))

;; ============================================================================
;; Error Handling Utilities
;; ============================================================================

(define-syntax-rule (with-gi-error-handling body ...)
  "Execute body with enhanced GI error handling."
  (catch #t
    (lambda () body ...)
    (lambda (key . args)
      (case key
        ((gi-error)
         (format (current-error-port) "GI Error: ~a~%" args)
         #f)
        ((misc-error)
         (if (and (pair? args) (string-contains (car args) "GI"))
             (begin
               (format (current-error-port) "GI Repository Error: ~a~%" (cadr args))
               #f)
             (apply throw key args)))
        (else
         (apply throw key args))))))

;; ============================================================================
;; Initialization
;; ============================================================================

;; Auto-enable dependency loading by default
(auto-load-dependencies! #t)

;; Commonly used namespaces - preload if available
(for-each (lambda (ns)
            (catch #t
              (lambda () 
                (when (require-namespace ns #:lazy? #t)
                  (preload-namespace ns)))
              (lambda (key . args) 
                #f))) ; Ignore errors for optional namespaces
          '("GLib" "GObject" "Gio"))

;; ============================================================================
;; Module Documentation
;; ============================================================================

#|
This module provides a high-level Scheme interface to GObject Introspection
repositories. It includes:

1. Repository Management:
   - require-namespace, get-loaded-namespaces
   - Automatic dependency handling
   - Performance optimizations with caching

2. Info Lookup:
   - find-by-name, find-by-gtype
   - Type-specific filtering
   - Pattern-based searching

3. Enhanced Utilities:
   - Namespace summaries and content listing
   - Pretty printing functions
   - Error handling utilities

4. GOOPS Integration:
   - Type hierarchy for GI info objects
   - Object-oriented interface

Example usage:

  (use-modules (gi repository))
  
  ;; Load a namespace
  (require-namespace "Gtk" "4.0")
  
  ;; Find specific info
  (define widget-info (find-by-name "Gtk" "Widget"))
  
  ;; Explore namespace contents
  (display-namespace-summary "Gtk")
  
  ;; Search for patterns
  (search-by-pattern ".*Button.*" '("Gtk"))
|#