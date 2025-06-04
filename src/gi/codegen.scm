;; -*- scheme -*-
;; GI Library Code Generator
;; Generates Scheme-friendly library definitions from GIRepository

(define-module (gi codegen)
  #:use-module (gi repository)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 hash-table)
  #:export (generate-gi-module
            generate-gi-module-file
            generate-all-loaded-modules))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(define (kebab-case str)
  "Convert CamelCase to kebab-case"
  (string-join 
    (map string-downcase
         (string-split 
           (string-substitute str "([A-Z])" "-\\1" 'global)
           #\-))
    "-"))

(define (safe-symbol name)
  "Convert a name to a safe Scheme symbol"
  (string->symbol (kebab-case name)))

(define (gi-type-tag->scheme-type tag)
  "Convert GI type tag to Scheme type symbol"
  (case tag
    ((#.GI-TYPE-TAG-VOID) 'void)
    ((#.GI-TYPE-TAG-BOOLEAN) 'boolean)
    ((#.GI-TYPE-TAG-INT8) 'int8)
    ((#.GI-TYPE-TAG-UINT8) 'uint8)
    ((#.GI-TYPE-TAG-INT16) 'int16)
    ((#.GI-TYPE-TAG-UINT16) 'uint16)
    ((#.GI-TYPE-TAG-INT32) 'int32)
    ((#.GI-TYPE-TAG-UINT32) 'uint32)
    ((#.GI-TYPE-TAG-INT64) 'int64)
    ((#.GI-TYPE-TAG-UINT64) 'uint64)
    ((#.GI-TYPE-TAG-FLOAT) 'float)
    ((#.GI-TYPE-TAG-DOUBLE) 'double)
    ((#.GI-TYPE-TAG-GTYPE) 'gtype)
    ((#.GI-TYPE-TAG-UTF8) 'utf8-string)
    ((#.GI-TYPE-TAG-FILENAME) 'filename-string)
    ((#.GI-TYPE-TAG-ARRAY) 'array)
    ((#.GI-TYPE-TAG-INTERFACE) 'interface)
    ((#.GI-TYPE-TAG-GLIST) 'glist)
    ((#.GI-TYPE-TAG-GSLIST) 'gslist)
    ((#.GI-TYPE-TAG-GHASH) 'ghash)
    ((#.GI-TYPE-TAG-ERROR) 'error)
    ((#.GI-TYPE-TAG-UNICHAR) 'unichar)
    (else 'unknown)))

(define (gi-direction->scheme direction)
  "Convert GI direction to Scheme direction"
  (case direction
    ((#.GI-DIRECTION-IN) 'input)
    ((#.GI-DIRECTION-OUT) 'output)
    ((#.GI-DIRECTION-INOUT) 'inout)
    (else 'unknown)))

(define (gi-transfer->scheme transfer)
  "Convert GI transfer mode to Scheme transfer"
  (case transfer
    ((#.GI-TRANSFER-NOTHING) 'nothing)
    ((#.GI-TRANSFER-CONTAINER) 'container)
    ((#.GI-TRANSFER-EVERYTHING) 'everything)
    (else 'unknown)))

(define (sanitize-module-name namespace version)
  "Create a safe module name from namespace and version"
  (string-append 
    (kebab-case namespace) 
    "-" 
    (string-map (lambda (c) (if (char=? c #\.) #\- c)) version)))

;; ============================================================================
;; Type Info Extraction
;; ============================================================================

(define (extract-type-info type-info)
  "Extract type information from a GITypeInfo"
  (let ((tag (gi-type-info-get-tag type-info))
        (is-pointer (gi-type-info-is-pointer type-info)))
    
    (case tag
      ((#.GI-TYPE-TAG-INTERFACE)
       (let ((interface-info (gi-type-info-get-interface type-info)))
         `(arg-type . ,(case (gi-type interface-info)
                        ((#.GI-INFO-TYPE-OBJECT) 'GObject)
                        ((#.GI-INFO-TYPE-INTERFACE) 'GInterface)
                        ((#.GI-INFO-TYPE-STRUCT #.GI-INFO-TYPE-BOXED) 'GBoxed)
                        ((#.GI-INFO-TYPE-ENUM #.GI-INFO-TYPE-FLAGS) 'GEnum)
                        ((#.GI-INFO-TYPE-UNION) 'GUnion)
                        (else 'GInterface)))
         `(gtype-name . ,(gi-name interface-info))))
      
      ((#.GI-TYPE-TAG-ARRAY)
       `((arg-type . array)
         (array-type . ,(gi-type-info-get-array-type type-info))
         (element-type . ,(extract-type-info (gi-type-info-get-param-type type-info 0)))
         (zero-terminated . ,(gi-type-info-is-zero-terminated type-info))
         (fixed-size . ,(gi-type-info-get-array-fixed-size type-info))))
      
      ((#.GI-TYPE-TAG-GLIST #.GI-TYPE-TAG-GSLIST)
       `((arg-type . ,(if (= tag GI-TYPE-TAG-GLIST) 'glist 'gslist))
         (element-type . ,(extract-type-info (gi-type-info-get-param-type type-info 0)))))
      
      ((#.GI-TYPE-TAG-GHASH)
       `((arg-type . ghash)
         (key-type . ,(extract-type-info (gi-type-info-get-param-type type-info 0)))
         (value-type . ,(extract-type-info (gi-type-info-get-param-type type-info 1)))))
      
      (else
       `(arg-type . ,(gi-type-tag->scheme-type tag))))))

;; ============================================================================
;; Argument Info Extraction  
;; ============================================================================

(define (extract-arg-info arg-info index)
  "Extract argument information from a GIArgInfo"
  (let* ((name (gi-name arg-info))
         (type-info (gi-arg-info-get-type arg-info))
         (direction (gi-arg-info-get-direction arg-info))
         (transfer (gi-arg-info-get-ownership-transfer arg-info))
         (optional (gi-arg-info-is-optional arg-info))
         (nullable (gi-arg-info-may-be-null arg-info))
         (caller-allocates (gi-arg-info-is-caller-allocates arg-info)))
    
    `((name . ,name)
      (meta ,(extract-type-info type-info)
            (flags ,@(append
                      (if (gi-type-info-is-pointer type-info) '(ptr) '())
                      (list (gi-direction->scheme direction))))
            (item-size . 8) ; Default pointer size
            (transfer . ,(gi-transfer->scheme transfer)))
      (s-direction . ,(gi-direction->scheme direction))
      (tuple . singleton)
      (presence . ,(if optional 'optional 'required))
      (i . ,index)
      (c-input-pos . ,index)
      (s-input-pos . ,index))))

;; ============================================================================
;; Function Info Extraction
;; ============================================================================

(define (extract-function-info func-info)
  "Extract function information from a GIFunctionInfo"
  (let* ((name (gi-name func-info))
         (symbol (gi-function-info-get-symbol func-info))
         (flags (gi-function-info-get-flags func-info))
         (n-args (gi-callable-info-get-n-args func-info))
         (return-type (gi-callable-info-get-return-type func-info))
         (may-return-null (gi-callable-info-may-return-null func-info))
         (skip-return (gi-callable-info-skip-return func-info))
         (throws (gi-callable-info-can-throw-gerror func-info)))
    
    (let ((args (map (lambda (i)
                       (extract-arg-info 
                         (gi-callable-info-get-arg func-info i) i))
                     (iota n-args)))
          (method-name (if (logand flags GI-FUNCTION-IS-METHOD)
                          (kebab-case name)
                          name))
          (constructor? (logand flags GI-FUNCTION-IS-CONSTRUCTOR))
          (getter? (logand flags GI-FUNCTION-IS-GETTER))
          (setter? (logand flags GI-FUNCTION-IS-SETTER)))
      
      `(^function
        ',(gi-namespace func-info)
        ',(gi-container func-info) ; Parent type if method
        ',(safe-symbol method-name)
        ',(safe-symbol name)
        ',symbol
        '((name . ,name)
          (s-input-req . ,(length (filter 
                                    (lambda (arg) 
                                      (eq? (assq-ref arg 's-direction) 'input))
                                    args)))
          (c-input-len . ,n-args)
          ,@(if (not (null? args))
                `((pdata ,@args))
                '())
          ,@(if (not skip-return)
                `((return-val
                   (name . "%return")
                   (meta ,(extract-type-info return-type)
                         (flags ,@(append
                                   (if (gi-type-info-is-pointer return-type) '(ptr) '())
                                   '(out)))
                         (transfer . ,(gi-transfer->scheme 
                                       (gi-callable-info-get-caller-owns func-info))))
                   (s-direction . output)
                   (tuple . singleton)
                   (presence . required)
                   (i . 0)))
                '())
          ,@(if throws '((throws . #t)) '())
          ,@(if constructor? '((constructor . #t)) '())
          ,@(if getter? '((getter . #t)) '())
          ,@(if setter? '((setter . #t)) '()))))))

;; ============================================================================
;; Enum Info Extraction
;; ============================================================================

(define (extract-enum-info enum-info)
  "Extract enum information from a GIEnumInfo"
  (let* ((name (gi-name enum-info))
         (n-values (gi-enum-info-get-n-values enum-info))
         (values (map (lambda (i)
                        (let ((value-info (gi-enum-info-get-value enum-info i)))
                          `(,(safe-symbol (gi-name value-info)) 
                            . ,(gi-value-info-get-value value-info))))
                      (iota n-values)))
         (flags? (= (gi-type enum-info) GI-INFO-TYPE-FLAGS)))
    
    `(,(if flags? '^typed-flags '^untyped-enum)
      ',(string->symbol (string-append "%" name))
      ',(string->symbol (string-append "%" name))
      ',values)))

;; ============================================================================
;; Constant Info Extraction
;; ============================================================================

(define (extract-constant-info const-info)
  "Extract constant information from a GIConstantInfo"
  (let* ((name (gi-name const-info))
         (type-info (gi-constant-info-get-type const-info))
         (value (gi-constant-info-get-value const-info)))
    
    `(^constant
      ',(safe-symbol name)
      ',(extract-type-info type-info)
      ,value)))

;; ============================================================================
;; Struct Info Extraction
;; ============================================================================

(define (extract-struct-info struct-info)
  "Extract struct information from a GIStructInfo"
  (let* ((name (gi-name struct-info))
         (size (gi-struct-info-get-size struct-info))
         (alignment (gi-struct-info-get-alignment struct-info))
         (n-fields (gi-struct-info-get-n-fields struct-info))
         (n-methods (gi-struct-info-get-n-methods struct-info))
         (foreign? (gi-struct-info-is-foreign struct-info))
         (gtype-struct? (gi-struct-info-is-gtype-struct struct-info)))
    
    (let ((fields (map (lambda (i)
                         (let ((field-info (gi-struct-info-get-field struct-info i)))
                           `((name . ,(gi-name field-info))
                             (type . ,(extract-type-info (gi-field-info-get-type field-info)))
                             (offset . ,(gi-field-info-get-offset field-info))
                             (size . ,(gi-field-info-get-size field-info))
                             (flags . ,(gi-field-info-get-flags field-info)))))
                       (iota n-fields)))
          (methods (map (lambda (i)
                          (gi-name (gi-struct-info-get-method struct-info i)))
                        (iota n-methods))))
      
      `(^struct
        ',(safe-symbol name)
        '((size . ,size)
          (alignment . ,alignment)
          (foreign . ,foreign?)
          (gtype-struct . ,gtype-struct?)
          ,@(if (not (null? fields))
                `((fields ,@fields))
                '())
          ,@(if (not (null? methods))
                `((methods ,@methods))
                '()))))))

;; ============================================================================
;; Object Info Extraction
;; ============================================================================

(define (extract-object-info object-info)
  "Extract object information from a GIObjectInfo"
  (let* ((name (gi-name object-info))
         (parent (gi-object-info-get-parent object-info))
         (abstract? (gi-object-info-get-abstract object-info))
         (fundamental? (gi-object-info-get-fundamental object-info))
         (final? (gi-object-info-get-final object-info))
         (type-name (gi-object-info-get-type-name object-info))
         (type-init (gi-object-info-get-type-init object-info))
         (n-interfaces (gi-object-info-get-n-interfaces object-info))
         (n-methods (gi-object-info-get-n-methods object-info))
         (n-properties (gi-object-info-get-n-properties object-info))
         (n-signals (gi-object-info-get-n-signals object-info)))
    
    (let ((interfaces (map (lambda (i)
                             (gi-name (gi-object-info-get-interface object-info i)))
                           (iota n-interfaces)))
          (methods (map (lambda (i)
                          (gi-name (gi-object-info-get-method object-info i)))
                        (iota n-methods)))
          (properties (map (lambda (i)
                             (gi-name (gi-object-info-get-property object-info i)))
                           (iota n-properties)))
          (signals (map (lambda (i)
                          (gi-name (gi-object-info-get-signal object-info i)))
                        (iota n-signals))))
      
      `(^object
        ',(safe-symbol name)
        '((parent . ,(if parent (gi-name parent) #f))
          (abstract . ,abstract?)
          (fundamental . ,fundamental?)
          (final . ,final?)
          (type-name . ,type-name)
          (type-init . ,type-init)
          ,@(if (not (null? interfaces))
                `((interfaces ,@interfaces))
                '())
          ,@(if (not (null? methods))
                `((methods ,@methods))
                '())
          ,@(if (not (null? properties))
                `((properties ,@properties))
                '())
          ,@(if (not (null? signals))
                `((signals ,@signals))
                '()))))))

;; ============================================================================
;; Interface Info Extraction
;; ============================================================================

(define (extract-interface-info interface-info)
  "Extract interface information from a GIInterfaceInfo"
  (let* ((name (gi-name interface-info))
         (n-prerequisites (gi-interface-info-get-n-prerequisites interface-info))
         (n-methods (gi-interface-info-get-n-methods interface-info))
         (n-properties (gi-interface-info-get-n-properties interface-info))
         (n-signals (gi-interface-info-get-n-signals interface-info)))
    
    (let ((prerequisites (map (lambda (i)
                                (gi-name (gi-interface-info-get-prerequisite interface-info i)))
                              (iota n-prerequisites)))
          (methods (map (lambda (i)
                          (gi-name (gi-interface-info-get-method interface-info i)))
                        (iota n-methods)))
          (properties (map (lambda (i)
                             (gi-name (gi-interface-info-get-property interface-info i)))
                           (iota n-properties)))
          (signals (map (lambda (i)
                          (gi-name (gi-interface-info-get-signal interface-info i)))
                        (iota n-signals))))
      
      `(^interface
        ',(safe-symbol name)
        '(,@(if (not (null? prerequisites))
                `((prerequisites ,@prerequisites))
                '())
          ,@(if (not (null? methods))
                `((methods ,@methods))
                '())
          ,@(if (not (null? properties))
                `((properties ,@properties))
                '())
          ,@(if (not (null? signals))
                `((signals ,@signals))
                '()))))))

;; ============================================================================
;; Main Info Dispatch
;; ============================================================================

(define (extract-info-data info)
  "Extract data from any GI info type"
  (case (gi-type info)
    ((#.GI-INFO-TYPE-FUNCTION)
     (extract-function-info info))
    ((#.GI-INFO-TYPE-ENUM #.GI-INFO-TYPE-FLAGS)
     (extract-enum-info info))
    ((#.GI-INFO-TYPE-CONSTANT)
     (extract-constant-info info))
    ((#.GI-INFO-TYPE-STRUCT #.GI-INFO-TYPE-BOXED)
     (extract-struct-info info))
    ((#.GI-INFO-TYPE-OBJECT)
     (extract-object-info info))
    ((#.GI-INFO-TYPE-INTERFACE)
     (extract-interface-info info))
    ;; TODO: Add other types as needed
    (else #f)))

;; ============================================================================
;; Module Generation
;; ============================================================================

(define* (generate-gi-module namespace #:optional version #:key (output-port #f))
  "Generate a complete Scheme module for a GI namespace"
  (let ((port (or output-port (current-output-port))))
    
    ;; Ensure namespace is loaded
    (require-namespace namespace version)
    
    (let* ((module-name (sanitize-module-name namespace (or version "1.0")))
           (all-infos (get-all-infos namespace))
           (exported-symbols '())
           (definitions '()))
      
      ;; Generate module header
      (format port "(define-module (gi ~a)~%" module-name)
      (format port "  #:use-module (gi runtime)~%")
      (format port "  #:export (")
      
      ;; Process all infos
      (for-each 
        (lambda (info)
          (when (not (gi-deprecated? info))
            (let ((data (extract-info-data info)))
              (when data
                (set! definitions (cons data definitions))
                (set! exported-symbols 
                      (cons (safe-symbol (gi-name info)) exported-symbols))))))
        all-infos)
      
      ;; Write exports
      (for-each (lambda (sym)
                  (format port "~%    ~a" sym))
                (reverse exported-symbols))
      (format port "))~%~%")
      
      ;; Write definitions
      (format port ";; Generated from ~a ~a~%" namespace (or version ""))
      (format port ";; Total definitions: ~a~%~%" (length definitions))
      
      (for-each (lambda (def)
                  (pretty-print def port)
                  (newline port))
                (reverse definitions))
      
      (format port ";; End of module (gi ~a)~%" module-name))))

(define* (generate-gi-module-file namespace #:optional version #:key (directory "."))
  "Generate a Scheme module file for a GI namespace"
  (let* ((module-name (sanitize-module-name namespace (or version "1.0")))
         (filename (format #f "~a/~a.scm" directory module-name)))
    
    (with-output-to-file filename
      (lambda ()
        (generate-gi-module namespace version #:output-port (current-output-port))))
    
    (format #t "Generated module file: ~a~%" filename)
    filename))

(define* (generate-all-loaded-modules #:key (directory "."))
  "Generate module files for all loaded namespaces"
  (let ((namespaces (get-loaded-namespaces)))
    (map (lambda (ns)
           (catch #t
             (lambda ()
               (generate-gi-module-file ns #:directory directory))
             (lambda (key . args)
               (format (current-error-port) 
                       "Error generating module for ~a: ~a~%" ns args)
               #f)))
         namespaces)))

         