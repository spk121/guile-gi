;; Copyright (C) 2023 Michael L. Gran

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

;; Functions to help parse typelib files and create Guile libraries
(define-module (gi parser)
  #:use-module (srfi srfi-9)
  #:use-module (gi girepository)
  #:use-module (system foreign)
  #:use-module (gi parser entry)
  #:use-module (gi parser constant)
  #:use-module (gi parser struct)
  #:use-module (gi parser union)
  #:use-module (gi parser enum)
  #:use-module (gi parser bitfield)
  #:export (<entry>
            get-declaration
            get-definition
            get-filtered-infos
            parse-infos))

(define (flatten lst)
  "Given a list where list elements may themselves be lists,
this pulls together all list and sublist elements into a single
list."
  ;; FIXME: inefficient
  (if (null? lst)
      lst
      ;; else
      (let loop ((cur (car lst))
                 (rest (cdr lst))
                 (output '()))
        (let ((new-output
               (if (list? cur)
                   (append output (flatten cur))
                   (append output (list cur)))))
          (if (null? rest)
              new-output
              ;; else
              (loop (car rest)
                    (cdr rest)
                    new-output))))))


(define (get-filtered-infos namespace allow-list deny-list)
  "Given a namespace string, such as 'Gtk', and two lists of strings,
and allow-list and a deny-list, this procedure fetches the infos for
the namespace from the irepository.  If the allow-list is not empty,
only infos whose names match entries in that list are returned. If the
deny-list is not empty, infos whose names match entries in that list
are not returned."
  (let ((infos (get-infos namespace)))
    (filter
     (lambda (info)
       (let ((name (base-info-get-name info)))
	     (and (or (null? allow-list)
				  (member name allow-list))
			  (or (null? deny-list)
				  (not (member name deny-list))))))
     infos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (callable-info-get-arg-type-infos info)
  "Given a callable <gibaseinfo>, this procedure returns a list of the
type <gibaseinfo> associated with the callable's arguments."
  (map-in-order
   (lambda (i)
     (arg-info-get-type i))
   (callable-info-get-args info)))

(define (callable-info-get-interface-infos info)
  "Given a callable <gibaseinfo>, it returns a list of infos of
interface types used by the callable's arguments and return value:
e.g. a list of structs, boxes, and objects used by the callable's
arguments.  The list may be empty if it only uses basic types."
  (let ((interface-args
         (filter
          (lambda (x)
            (eqv? 'interface (type-info-get-tag x)))
          (cons (callable-info-get-return-type info)
                (callable-info-get-arg-type-infos info)))))
    (map type-info-get-interface interface-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (type-tag-to-guile-ffi-arg-type typetag)
  (unless (symbol? typetag)
    (scm-error 'wrong-type-arg "type-tag-to-guile-ffi-arg-type"
               "Not a symbol: ~S" (list typetag) (list typetag)))
  (cond
   ((eq? typetag 'void) 'void)
   ((eq? typetag 'boolean) 'int)               ; GLib's gboolean is gint
   ((eq? typetag 'int8) 'int8)
   ((eq? typetag 'uint8) 'uint8)
   ((eq? typetag 'int16) 'int16)
   ((eq? typetag 'uint16) 'uint16)
   ((eq? typetag 'int32) 'int32)
   ((eq? typetag 'uint32) 'uint32)
   ((eq? typetag 'int64) 'int64)
   ((eq? typetag 'uint64) 'uint64)
   ((eq? typetag 'float) 'float)
   ((eq? typetag 'double) 'double)
   ((eq? typetag 'gtype) 'size_t)
   ((member typetag '(utf8 filename array interface glist gslist ghash error)) '*)
   ((eq? typetag 'unichar) 'uint32)
   (else
    (error "unknown type tag"))))

(define (type-info-to-guile-ffi-arg-type typeinfo)
  (assert-gitypeinfo "type-info-to-guile-ffi-arg-type" typeinfo)
  (if (type-info-is-pointer? typeinfo)
      '*
      ;; else
      (type-tag-to-guile-ffi-arg-type
       (type-info-get-tag typeinfo))))

(define (func-info-to-guile-ffi-declaration funcinfo dllib)
  "Given a function <gibaseinfo>, create the (system foreign) function
declaration."
  (assert-gifunctioninfo "func-info-to-guile-ffi-declaration" funcinfo)
  (unless (string? dllib)
    (scm-error 'wrong-type-arg "func-info-guile-ffi-declaration"
               "Not a string: ~S" (list dllib) (list dllib)))

  (let* ((c-name-string (function-info-get-symbol funcinfo))
         (c-name-symbol (string->symbol c-name-string))
         (self? (member 'method (function-info-get-flags funcinfo)))
         (guile-return-arg-type
          (type-info-to-guile-ffi-arg-type
           (callable-info-get-return-type funcinfo)))
         (guile-arg-types
          (map
           type-info-to-guile-ffi-arg-type
           (callable-info-get-arg-type-infos funcinfo))))
    (when self?
      (set! guile-arg-types (append! (list '*) guile-arg-types)))
    `(define ,c-name-symbol
       (foreign-library-function ,dllib ,c-name-string
                                 ,@(if (eq? guile-return-arg-type void)
                                       '()
                                       `(#:return-type ,guile-return-arg-type))
                                 ,@(if (null? guile-arg-types)
                                       '()
                                       `(#:arg-types ,guile-arg-types))))))
(define (func-il-type info)
  (assert-gifunctioninfo "func-il-type" info)
  (let ((flags (function-info-get-flags info)))
    (cond
     ((member 'constructor flags)
      '^constructor)
     ((member 'method flags)
      '^method)
     (else
      '^function))))

(define (func-il-param arginfo)
  (let* ((typeinfo (arg-info-get-type arginfo))
         (typetag (type-info-get-tag typeinfo))
         (typestr (cond
                   ((eqv? typetag 'interface)
                    (base-info-get-name (type-info-get-interface typeinfo)))
                   (else
                    (type-tag-to-string typetag)))))
    
    `((name . ,(base-info-get-name arginfo))
      (transfer-ownership . ,(arg-info-get-ownership-transfer arginfo))
      (nullable . ,(arg-info-may-be-null? arginfo))
      (type . ,(string->symbol typestr)))))

(define (func-il-return callable-info)
  (let* ((typeinfo (callable-info-get-return-type callable-info))
         (typetag (type-info-get-tag typeinfo))
         (typestr (cond
                   ((eqv? typetag 'interface)
                    (base-info-get-name (type-info-get-interface typeinfo)))
                   (else
                    (type-tag-to-string typetag)))))
    
    `((caller-owns . ,(callable-info-get-caller-owns callable-info))
      (nullable . ,(callable-info-may-return-null? callable-info))
      (type . ,(string->symbol typestr)))))

(define (func-il-params info)
  "Creates the IL segment for a list of callable parameters."
  (map func-il-param (callable-info-get-args info)))

(define* (parse-function-info info namespace #:optional self)
  ;; Any types that this function depends on must be loaded
  ;; first.
  (let* ((arg-entries
          (parse-infos (callable-info-get-interface-infos info) namespace))
         (flags (function-info-get-flags info))
         (return-il (func-il-return info))
         (param-ils (func-il-params info)))
    
    ;; Information about function arguments and return types are not
    ;; available at runtime, so we gather then now.
    (append
     arg-entries
     (list
      (make-entry
       ;; Declaration.
       (list (string->symbol (base-info-get-name info)))
       ;; Definition
       (append
        (func-info-to-guile-ffi-declaration info "gtk")

       `(,(func-il-type info)
         (
          ,@(if self (list (cons 'self self)) '()) ; look! I used
                                        ; unquote
                                        ; spicing. Fancy.
          (name . ,(base-info-get-name info))
          (symbol . ,(function-info-get-symbol info))
          (return-value . ,return-il)
          (params . ,param-ils)
          ))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-callback-info info namespace)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRUCT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(define (parse-struct-info info namespace)
  ;; 
  (list
   (make-entry
    ;; Declaration.
    (list (string->symbol (base-info-get-name info)))
    `(^struct ,(base-info-get-name info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOXED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-boxed-info info namespace)
  (list
   (make-entry
    ;; Declaration.
    (list (string->symbol (base-info-get-name info)))
    `(^boxed ,(base-info-get-name info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENUM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(define (parse-enum-info info namespace)
  ;; If an enum has a GType, the values can be determined at runtime.
  ;; If it has no GType, they are determined now.
  (if (> (registered-type-info-get-g-type info) 4)
      (parse-typed-enum-info info namespace)
      (parse-untyped-enum-info info namespace)))

(define (parse-untyped-enum-info info namespace)
  (list
   (make-entry
    ;; Declaration.
    (list (string->symbol (base-info-get-name info)))

    ;; Definition.
    ;; Since we have to do this at parse-time, not run time, we
    ;; can construct the whole class now.
    (let ((values-alist (enum-values->alist info))
          (class-name (string->symbol (base-info-get-name info))))
      ;; #:class == one hash table for all instances.
      `(define-class ,class-name (<GEnum>)
         (obarray #:allocation #:class
                  #:init-value (alist->hashq-table ,values-alist)))))))

(define (parse-typed-enum-info info namespace)
  (list
   (make-entry
    ;; Declaration.
    (list (string->symbol (base-info-get-name info)))

    ;; Definition.
    ;; At run time, we can get the names and values from the GEnumClass
    ;; struct.
    `(^typed-enum ,(base-info-get-name info)))))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BITFIELD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(define (parse-bitfield-info info namespace)
  ;; If a bitfield has a GType, the values can be extracted at runtime.
  ;; If it has no GType, they are determined now.
  (if (> (registered-type-info-get-g-type info) 4)
      (parse-typed-bitfield-info info namespace)
      (parse-untyped-bitfield-info info namespace)))

(define (parse-typed-bitfield-info info namespace)
  (list
   (make-entry
    ;; Declaration.
    (list (string->symbol (base-info-get-name info)))

    ;; Definition.
    ;; At run time, we can get the names and values from the GFlagsClass
    ;; struct.
    `(^typed-bitfield ,(base-info-get-name info)))))

(define (parse-untyped-bitfield-info info namespace)
  (list
   (make-entry
    ;; Declaration.
    (list (string->symbol (base-info-get-name info)))

    ;; Definition.
    ;; Since we have to do this at parse-time, not run time, we
    ;; can construct the whole class now.
    (let ((values-alist (enum-values->alist info))
          (class-name (string->symbol (base-info-get-name info))))
      ;; #:class == one hash table for all instances.
      `(define-class ,class-name (<GBitfield>)
         (obarray #:allocation #:class
                  #:init-value (alist->hashq-table ,values-alist)))))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (obj-method-infos info)
  (let ((n (object-info-get-n-methods info))
        (infos '()))
    (do ((i 0 (1+ i))) ((>= i n))
      (set! infos (append! infos (list (object-info-get-method info i)))))
    infos))

(define (obj-parent-entries info namespace)
  (let ((parent (object-info-get-parent info)))
    (if parent
        (parse-info parent namespace)
        '())))
  
(define (parse-object-info info namespace)
  ;; Parent needs to be defined first
  (let* ((parent-entries (obj-parent-entries info namespace))
         (entry
          (make-entry
           ;; Declaration.
           (list (string->symbol (base-info-get-name info)))
           
           ;; Definition
           `(^object ((name . ,(base-info-get-name info))
                      (gtype-name . ,(registered-type-info-get-type-name info))
                      )) 
           ;; fields
           ;; constants
           ;; set value func
           ;; get value func
           ;; signals
           ;; methods
           ;; constructors
           
           ;; ref function is runtime
           ;; unref function is runtime
           ;; properties are runtime
           ))
         (method-entries (parse-infos (obj-method-infos info)
                                      namespace
                                      (base-info-get-name info))))
    (append parent-entries (list entry) method-entries)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-interface-info info namespace)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-constant-info info namespace)
  ;; Since a constant has no GType, its value gets defined now.
  (list
   (make-entry
    ;; Declaration
    (list (string->symbol (base-info-get-name info)))

    ;; Definition
    `(define ,(string->symbol (base-info-get-name info))
       ,(constant-info-get-value info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(define (parse-union-info info namespace)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VALUE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-value-info info namespace)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIGNAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-signal-info info namespace)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFUNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-vfunc-info info namespace)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROPERTY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-property-info info namespace)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIELD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-field-info info namespace)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-arg-info info namespace)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-type-info info namespace)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE TOP
;; Let's go.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (parse-infos infos namespace #:optional X)
  "Given a <gibaseinfo>, this returns a list of <entry> types
that have the scheme declaration and definition for the info."
  (flatten (map-in-order
            (lambda (info)
              (parse-info info namespace))
              infos)))

(define (parse-noop info)
  '())

(define parse-dispatch-table
  `((invalid . ,parse-noop)
    (function . ,parse-function-info)
    (callback . ,parse-callback-info)
    (struct . ,parse-struct-info)
    (boxed . ,parse-boxed-info)
    (enum . ,parse-enum-info)
    (flags . ,parse-bitfield-info)
    (object . ,parse-object-info)
    (interface . ,parse-interface-info)
    (constant . (list constant-info-parents parse-constant-info constant-info-children))
    (invalid-0 . ,parse-noop)
    (union . ,parse-union-info)
    (value . ,parse-value-info)
    (signal . ,parse-signal-info)
    (vfunc . ,parse-vfunc-info)
    (property . ,parse-property-info)
    (field . ,parse-field-info)
    (arg . ,parse-arg-info)
    (type . ,parse-type-info)
    (unresolved . ,parse-noop)))

(define *completed* (make-hash-table))

(define* (parse-info info namespace)
  "Given a <gibaseinfo>, this procedure returns a list of <entry> types
which contain the export declaration and the definition of the info."
  (format #t "Pondering ~S~%" (base-info-get-name info))
  (let ((key (string->symbol (string-append (base-info-get-namespace info)
                                            (base-info-get-name info)))))
    (cond
     ((not (string=? namespace (base-info-get-namespace info)))
      '())
     ((hash-ref *completed* key)
      '())
     ((eqv? 'constant (base-info-get-type info))
      (hash-set! *completed* key #t)
      (parse-constant-info info namespace))
     ((eqv? 'struct (base-info-get-type info))
      (hash-set! *completed* key #t)
      (append
       (parse-struct-info info namespace)
       (parse-infos (struct-info-children info) namespace)))
     ((eqv? 'union (base-info-get-type info))
      (hash-set! *completed* key #t)
      (append
       (parse-union-info info namespace)
       (parse-infos (union-info-children info) namespace)))
     ((eqv? 'enum (base-info-get-type info))
      (hash-set! *completed* key #t)
      (append
       (parse-enum-info info namespace)
       (parse-infos (enum-info-children info) namespace)))
     ((eqv? 'flags (base-info-get-type info))
      (hash-set! *completed* key #t)
      (append
       (parse-bitfield-info info namespace)
       (parse-infos (bitfield-info-children info) namespace)))
     (else
      (hash-set! *completed* key #t)
      (list
       (make-entry (list (string->symbol (base-info-get-name info)))
                   `(define ,(string->symbol (base-info-get-name info)) 'tbd ,(base-info-get-type info))))))))

#|
      (hash-set! *completed* key #t)
      
      (let* ((op (assoc-ref parse-dispatch-table (base-info-get-type info)))
             (get-parents (car op))
             (parse (cadr op))
             (get-children (caddr op)))
        (append
         (parse-infos (get-parents info namespace) namespace)
         (parse info namespace)
         (parse-infos (get-children info namespace) namespace)))))))
|#
