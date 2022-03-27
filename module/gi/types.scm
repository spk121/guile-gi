;; Copyright (C), 2019 Michael L. Gran

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

(define-module (gi types)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)

  #:use-module (gi oop)

  #:re-export (<GBoxed> <GEnum> <GFlags>)
  #:export (<GObject> <GInterface> <GParam> <GVariant>
            <GValue> transform
            <GClosure> procedure->closure
            enum->number enum->symbol number->enum symbol->enum
            flags->number flags->list number->flags list->flags flags-set?
            enum-universe
            flags-mask flags-union flags-intersection flags-difference
            flags-complement flags-projection flags-projection/list
            flags-projection/number
            is-registered-callback?
            get-registered-callback-closure-pointer

            ;;
            G_TYPE_NONE
            G_TYPE_CHAR
            G_TYPE_UCHAR
            G_TYPE_BOOLEAN
            G_TYPE_ENUM
            G_TYPE_OBJECT
            G_TYPE_POINTER
            G_TYPE_FLAGS
            G_TYPE_STRING
            G_TYPE_INT
            G_TYPE_UINT
            G_TYPE_LONG
            G_TYPE_ULONG
            G_TYPE_INT64
            G_TYPE_UINT64
            G_TYPE_FLOAT
            G_TYPE_DOUBLE
            get-gtype
            gtype-get-scheme-type
            gtype-get-name
            gtype-get-parent
            gtype-get-fundamental
            gtype-get-children
            gtype-get-interfaces
            gtype-get-depth
            gtype-is-interface?
            gtype-is-classed?
            gtype-is-instantiatable?
            gtype-is-derivable?
            gtype-is-a?
            %gtype-dump-table))

(eval-when (expand load eval)
  (load-extension "libguile-gi" "gig_init_types")
  (load-extension "libguile-gi" "gig_init_value")
  (load-extension "libguile-gi" "gig_init_closure")
  (load-extension "libguile-gi" "gig_init_callback"))

;;(define +debug-port+ (current-output-port))
(define +debug-port+ (%make-void-port "w"))

(define (type-register-self gtype stype)
  (let ((parent ($type-parent gtype))
        (old-stype (hashq-ref %gtype-hash gtype)))
    (when (or (not old-stype) (not (eq? stype old-stype)))
      (when (or (not old-stype) (and (eq? old-stype <unknown>) (not (eq? stype <unknown>))))
        (hashq-set! %gtype-hash gtype stype)
        (if old-stype
            (if (zero? parent)
                (format +debug-port+
                        "~S : re-registering a type for ~x as ~S~%"
                        ($type-name gtype)
                        gtype
                        stype)
                (format +debug-port+
                        "~S : re-registering a ~A type for ~x as ~S~%"
                        ($type-name gtype)
                        ($type-name parent)
                        gtype
                        stype))
            (if (zero? parent)
                (format +debug-port+
                        "~S : registering a new type for ~x as ~S~%"
                        ($type-name gtype)
                        gtype
                        stype)
                (format +debug-port+
                        "~S : registering a new ~A type for ~x as ~S~%"
                        ($type-name gtype)
                        ($type-name parent)
                        gtype
                        stype)))))))

(define (type-register gtype stype)
  "Make the unidirectional gtype -> stype association. This does not
require that the stype -> gtype association is one-to-one."
  (let ((parent ($type-parent gtype)))
    (unless (zero? parent)
      (type-register-self parent <unknown>))
    (type-register-self gtype stype)))

(define (type-associate gtype stype)
  "Make a bidirectional gtype <-> stype association. Use this when the
scheme class only maps to a single GType."
  (type-register gtype stype)
  (set-object-property! stype 'sort-key
                        (hash-count (const #t) %gtype-hash))
  (hashq-set! %reverse-hash stype gtype))

(define (type-is-registered? gtype)
  (not (not (hashq-ref %gtype-hash gtype))))

(define (type-class-name-from-gtype gtype)
  (string-append "<"
                 ($type-name gtype)
                 ">"))

(define (type-define-fundamental gtype supers ref unref)
  (if (type-is-registered? gtype)
      (begin
        (format +debug-port+
                "not redefining fundamental type ~S"
                ($type-name gtype))
        (hashq-ref %gtype-hash gtype))
      ;; else
      (%make-fundamental-class
       (string->symbol (type-class-name-from-gtype gtype))
       supers ref unref)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some associations of extant scheme types

(type-register G_TYPE_NONE <unknown>)

(type-associate G_TYPE_STRING <string>)
(type-register G_TYPE_INT <integer>)
(type-register G_TYPE_UINT <integer>)
(type-register G_TYPE_LONG <integer>)
(type-register G_TYPE_ULONG <integer>)
(type-register G_TYPE_INT64 <integer>)
(type-register G_TYPE_UINT64 <integer>)
(type-register G_TYPE_FLOAT <real>)
(type-register G_TYPE_DOUBLE <real>)
(type-register G_TYPE_CHAR <char>)
(type-register G_TYPE_UCHAR <char>)
(type-associate G_TYPE_BOOLEAN <boolean>)
(type-register G_TYPE_POINTER <foreign>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental GObject type classes

(type-associate G_TYPE_BOXED <GBoxed>)
(type-associate G_TYPE_ENUM <GEnum>)
(type-associate G_TYPE_FLAGS <GFlags>)

(define <GObject>
  (type-define-fundamental G_TYPE_OBJECT '() $object-ref-sink-ptr
                           $object-unref-ptr))
(type-associate G_TYPE_OBJECT <GObject>)

(define <GInterface>
  (type-define-fundamental G_TYPE_INTERFACE '() $null-ptr $null-ptr))
(type-associate G_TYPE_INTERFACE <GInterface>)

(define <GParam>
  (type-define-fundamental G_TYPE_PARAM `(,<applicable-struct-with-setter>)
                           $param-spec-ref-sink-ptr
                           $param-spec-unref-ptr))
(type-associate G_TYPE_PARAM <GParam>)

(define <GVariant>
  (type-define-fundamental G_TYPE_VARIANT '()
                           $variant-ref-sink-ptr
                           $variant-unref-ptr))
(type-associate G_TYPE_VARIANT <GVariant>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core types that require GBoxed be ready first

(define <GValue> ($make-type-with-gtype G_TYPE_VALUE `(,<applicable-struct-with-setter>) SIZEOF_GVALUE))
(type-associate G_TYPE_VALUE <GValue>)

(define <GClosure> ($make-type-with-gtype G_TYPE_CLOSURE `(,<applicable-struct>)))
(type-associate G_TYPE_CLOSURE <GClosure>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%gtype-dump-table)
  "Returns a list describing the current state of the GType to Scheme
class mapping. Each entry is the GType, the GType name, and the scheme
class."
  (hash-map->list
   (lambda (key val)
     (list key (gtype-get-name key) val))
   %gtype-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define (type-check-typed-object obj expected-type)
  (is-a? obj expected-type))

(define (type-peek-typed-object obj expected-type)
  (if (not (is-a? obj expected-type))
      %null-pointer
      ;; else
      (slot-ref obj 'value)))

(define (type-peek-object obj)
  (type-peek-typed-object obj <GFundamental>))

(define (type-check-object? obj)
  (is-a? obj <GFundamental>))
|#

;;; Values and Params

(define-method (initialize (value <GValue>) initargs)
  (next-method)
  (slot-set! value 'procedure (cut %get value))
  (slot-set! value 'setter
             (case-lambda
               ((v) (%set! value v))
               ((t v) (%set-type! value t) (%set! value v)))))

(define-method (transform (value <GValue>) gtype)
  (%transform value gtype))

(define-method (initialize (pspec <GParam>) initargs)
  (next-method)
  (slot-set! pspec 'procedure (cut (@@ (gi oop) %get-property) <> pspec))
  (slot-set! pspec 'setter (cut (@@ (gi oop) %set-property!) <> pspec <>)))

;;; Closures

(define-method (initialize (closure <GClosure>) initargs)
  (next-method)
  (slot-set! closure 'procedure
             (match-lambda*
               (((type . out-mask) args ...)
                (%invoke-closure closure type out-mask args))
               ((type args ...)
                (%invoke-closure closure type #f args)))))

;;; Enum conversions

(define-method (enum->symbol (enum <GEnum>))
  (let ((expected (slot-ref enum 'value)))
    (hash-fold (lambda (key value seed)
                 (if (= value expected) key seed))
               #f (slot-ref enum 'obarray))))

(define-method (enum->symbol (class <class>) (enum <GEnum>))
  (if (is-a? enum class)
      (enum->symbol enum)
      (scm-error 'wrong-type-arg "enum->symbol" "expected ~a"
                 (list (class-name class)) (list enum))))

(define-method (enum->symbol (class <class>))
  (lambda (enum) (enum->symbol class enum)))

(define-method (enum->number (number <number>))
  (format (current-error-port) "WARNING: passing number ~a as enum~%" number)
  (display-backtrace (make-stack #t 1) (current-error-port))
  number)

(define-method (enum->number (enum <GEnum>))
  (slot-ref enum 'value))

(define-method (enum->number (class <class>) (enum <GEnum>))
  (if (is-a? enum class)
      (enum->number enum)
      (scm-error 'wrong-type-arg "enum->number" "expected ~a"
                 (list (class-name class)) (list enum))))

(define-method (enum->number (class <class>) (symbol <symbol>))
  (hashq-ref (class-slot-ref class 'obarray) symbol))

(define-method (enum->number (class <class>))
  (lambda (symbol) (enum->number class symbol)))

(define-method (symbol->enum (class <class>) (symbol <symbol>))
  (or (and-let* ((obarray (class-slot-ref class 'obarray))
                 (value (hashq-ref obarray symbol)))
        (make class #:value value))
      (scm-error 'out-of-range "symbol->enum"
                 "not defined in ~A" (list class) (list symbol))))

(define-method (symbol->enum (class <class>))
  (lambda (symbol) (symbol->enum class symbol)))

(define-method (number->enum (class <class>) (number <number>))
  (or
   (hash-fold
    (lambda (key value seed)
      (if (= value number) (make class #:value value) seed))
    #f (class-slot-ref class 'obarray))
   (scm-error 'out-of-range "number->enum"
              "not bound in ~A" (list class) (list symbol))))

(define-method (number->enum (class <class>))
  (lambda (number) (number->enum class number)))

;;; Flag conversions

(define-method (flags->number (number <number>))
  (format (current-error-port) "WARNING: passing number ~a as flags~%" number)
  (display-backtrace (make-stack #t 1) (current-error-port))
  number)

(define-method (flags->number (flags <GFlags>))
  (slot-ref flags 'value))

(define-method (number->flags (class <class>) (number <number>))
  ((lambda (flags)
     (if (= (flags->number flags) number)
         flags
         (scm-error 'out-of-range "number->flags"
                    "strange flags seem set" '() (list number))))
   (make class
     #:value
     (hash-fold
      (lambda (key value seed)
        (if (= (logand number value) value)
            (logior value seed)
            seed))
      0 (class-slot-ref class 'obarray)))))

(define-method (number->flags (class <class>))
  (lambda (number) (number->flags class number)))

(define-method (flags-set? (flags <GFlags>) (number <number>))
  (= (logand (slot-ref flags 'value) number) number))

(define-method (flags-set? (flags <GFlags>) (symbol <symbol>))
  (flags-set? flags (hashq-ref (slot-ref flags 'obarray) symbol)))

(define-method (flags-set? (flags <GFlags>) (list <list>))
  (every (lambda (f) (flags-set? flags f)) list))

(define-method (flags->list (flags <GFlags>))
  (hash-fold
   (lambda (key value seed)
     (if (flags-set? flags value) (cons key seed) seed))
   '() (slot-ref flags 'obarray)))

(define-method (flags->list (class <class>) (flags <GFlags>))
  (if (is-a? flags class)
      (flags->list flags)
      (scm-error 'wrong-type-arg "flags->list" "expected ~a"
                 (list (class-name class)) (list flags))))

(define-method (flags->list (class <class>))
  (lambda (flags) (flags->list class flags)))

(define-method (list->flags (class <class>) (lst <list>))
  (let* ((obarray (class-slot-ref class 'obarray))
         (lookup (lambda (symbol)
                   (or (hashq-ref obarray symbol)
                       (scm-error 'out-of-range "list->flags"
                                  "not defined in ~A" (list class)
                                  (list symbol))))))
    (make class #:value (apply logior (map lookup lst)))))

(define-method (list->flags (class <class>))
  (lambda (flags) (list->flags class flags)))

(define-method (flags->number (class <class>) (flags <GFlags>))
  (if (is-a? flags class)
      (flags->number flags)
      (scm-error 'wrong-type-arg "flags->number" "expected ~a"
                 (list (class-name class)) (list flags))))

(define-method (flags->number (class <class>) (list <list>))
  (flags->number (list->flags class list)))

(define-method (flags->number (class <class>))
  (lambda (list) (flags->number class list)))

;;; rnrs enums analogues

(define-method (enum-universe (class <class>))
  (hash-map->list (lambda (k v) k) (class-slot-ref class 'obarray)))

(define-method (enum-universe (enum <GEnum>))
  (enum-universe (class-of enum)))

(define-method (enum-universe (flags <GFlags>))
  (enum-universe (class-of flags)))

(define-method (flags-mask (class <class>))
  (make class #:value
        (hash-fold (lambda (key value seed) (logior value seed)) 0
                   (class-slot-ref class 'obarray))))

(define-method (flags-mask (flags <GFlags>))
  (flags-mask (class-of flags)))

(define-method (flags-union (flags1 <GFlags>) (flags2 <GFlags>))
  (if (equal? (class-of flags1) (class-of flags2))
      (make (class-of flags1) #:value
            (logior (slot-ref flags1 'value)
                    (slot-ref flags2 'value)))
      (error "cannot unite flags of differing type")))

(define-method (flags-union (car <GFlags>) . rest)
  (fold flags-union car rest))

(define-method (flags-intersection (flags1 <GFlags>) (flags2 <GFlags>))
  (if (equal? (class-of flags1) (class-of flags2))
      (make (class-of flags1) #:value
            (logand (slot-ref flags1 'value)
                    (slot-ref flags2 'value)))
      (error "cannot unite flags of differing type")))

(define-method (flags-intersection (car <GFlags>) . rest)
  (fold flags-intersection car rest))

(define-method (flags-difference (flags1 <GFlags>) (flags2 <GFlags>))
  (if (equal? (class-of flags1) (class-of flags2))
      (make (class-of flags1) #:value
            (logxor (slot-ref flags1 'value)
                    (slot-ref flags2 'value)))
      (error "cannot unite flags of differing type")))

(define-method (flags-complement (flags <GFlags>))
  (flags-difference flags (flags-mask flags)))

(define-method (flags-projection/list (flags <GFlags>) (class <class>))
  (let ((v (flags->list flags))
        (m (flags->list (flags-mask class))))
    (list->flags class (lset-intersection eq? v m))))

(define-method (flags-projection/list (flags <GFlags>) (flags2 <GFlags>))
  (flags-projection/list flags (class-of flags2)))

(define-method (flags-projection/number (flags <GFlags>) (class <class>))
  (let ((v (flags->number flags))
        (m (flags->number (flags-mask class))))
    (number->flags class (logand v m))))

(define-method (flags-projection/number (flags <GFlags>) (flags2 <GFlags>))
  (flags-projection/number flags (class-of flags2)))

(define flags-projection flags-projection/list)

;;; Enum/Flag printing

(define-method (display (enum <GEnum>) port)
  (display (enum->symbol enum) port))

(define-method (display (flags <GFlags>) port)
  (display (flags->list flags) port))

(define-method (write (enum <GEnum>) port)
  (format port "#<~s ~a>" (class-name (class-of enum)) (enum->symbol enum)))

(define-method (write (flags <GFlags>) port)
  (format port "#<~s ~a>" (class-name (class-of flags)) (flags->list flags)))

;;; Enum equality

(define-method (= (enum1 <GEnum>) (enum2 <GEnum>))
  (eq? (slot-ref enum1 'value) (slot-ref enum2 'value)))

(define-method (= (enum <GEnum>) (number <number>))
  (= (enum->number enum) number))

(define-method (= (number <number>) (enum <GEnum>))
  (= number (enum->number enum)))

(define-method (equal? (enum1 <GEnum>) (enum2 <GEnum>))
  (and (equal? (class-of enum1) (class-of enum2))
       (eq? (slot-ref enum1 'value) (slot-ref enum2 'value))))

;;; Flag equality

(define-method (= (flags <GFlags>) (number <number>))
  (= (slot-ref flags 'value) number))

(define-method (= (number <number>) (flags <GFlags>))
  (= number (slot-ref flags 'value)))

(define-method (= (flags1 <GFlags>) (flags2 <GFlags>))
  (= (slot-ref flags1 'value) (slot-ref flags2 'value)))

(define-method (equal? (flags1 <GFlags>) (flags2 <GFlags>))
  (and (equal? (class-of flags1) (class-of flags2))
       (= flags1 flags2)))
