;; Copyright (C) 2022 Michael L. Gran

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


;; Here we export everything allocated in the C runtime.

(define-module (gi runtime)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module ((gi core-generics) #:select (connect))
  #:export (;; main
            ^initialize
            ;; callback
            %before-c-callback-hook
            %before-callback-hook
            ;; closure
            procedure->closure
            %invoke-closure
            ;; constant
            ^constant
            ;; flag
            ^untyped-enum-conv
            ^untyped-flags-conv
            ^enum-conv
            ^flags-conv
            ;; function
            %before-function-hook
            ^function
            ^signal
            ;; glib
            ;; lib
            ^library
            ;; logging
            install-port-logger!
            install-journal-logger!
            install-custom-logger!
            ;; object
            %make-gobject
            %gobject-get-pspec
            %get-property
            %set-property!
            %connect
            %emit
            %define-object-type
            ^property
            ;; type
            G_TYPE_BOOLEAN
            G_TYPE_BOXED
            G_TYPE_CHAR
            G_TYPE_CLOSURE
            G_TYPE_DOUBLE
            G_TYPE_ENUM
            G_TYPE_FLAGS
            G_TYPE_FLOAT
            G_TYPE_INT
            G_TYPE_INT64
            G_TYPE_INTERFACE
            G_TYPE_LONG
            G_TYPE_NONE
            G_TYPE_OBJECT
            G_TYPE_PARAM
            G_TYPE_POINTER
            G_TYPE_STRING
            G_TYPE_UCHAR
            G_TYPE_UINT
            G_TYPE_UINT64
            G_TYPE_ULONG
            G_TYPE_VALUE
            G_TYPE_VARIANT
            SIZEOF_GVALUE
            SIZEOF_GCLOSURE
            type-<?
            ^type
            ^sized-type
            ^custom-type
            ^untyped-flags
            ^untyped-enum
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
            %allocate-boxed
            ;; signal
            ;; value
            %get
            %get-type
            %set!
            %set-type!
            %transform
            
            ;; Locally
            runtime-eval
            <GFundamental> <GBoxed> <GObject> <GInterface> <GParam>
            <GVariant>
            <GValue> transform
            <GClosure> procedure->closure
            <GEnum> <GFlags>
            enum->number enum->symbol number->enum symbol->enum
            flags->number flags->list number->flags list->flags flags-set?
            enum-universe
            flags-mask flags-union flags-intersection flags-difference
            flags-complement flags-projection flags-projection/list
            flags-projection/number
            is-registered-callback?
            get-registered-callback-closure-pointer

            <signal> make-signal
            connect connect-after
            ))

(eval-when (expand load eval)
  (load-extension "libguile-gi" "gig_init_stage1"))

(define *IL*
  '(
    ^constant
    ^enum-conv
    ^flags-conv
    ^function
    ^initialize
    ^library
    ^property
    ^signal
    ^type
    ^boxed-type
    ^custom-type
    ^untyped-enum
    ^untyped-enum-conv
    ^untyped-flags
    ^untyped-flags-conv
    ))

(define (runtime-eval expr)
  ;; (pk 'eval expr)
  (let ((sym (car expr))
        (args (cdr expr)))
    (let ((defs
            (cond
             ((eq? sym '^constant)
              (apply ^constant args))
             ((eq? sym '^custom-type)
              (apply ^custom-type args))
             ((eq? sym '^enum-conv)
              (apply ^enum-conv args))
             ((eq? sym '^flags-conv)
              (apply ^flags-conv args))
             ((eq? sym '^function)
              (apply ^function args))
             ((eq? sym '^initialize)
              (apply ^initialize args))
             ((eq? sym '^library)
              (apply ^library args))
             ((eq? sym '^property)
              (apply ^property args))
             ((eq? sym '^signal)
              (apply ^signal args))
             ((eq? sym '^sized-type)
              (apply ^sized-type args))
             ((eq? sym '^type)
              (apply ^type (pk 'ZOOP args)))
             ((eq? sym '^untyped-enum)
              (apply ^untyped-enum args))
             ((eq? sym '^untyped-enum-conv)
              (apply ^untyped-enum-conv args))
             ((eq? sym '^untyped-flags)
              (apply ^untyped-flags args))
             ((eq? sym '^untyped-flags-conv)
              (apply ^untyped-flags-conv args))
             (else
              (error "unknown expression '~S'" sym)))))
      (if (not (list? defs))
          (error "bad return from internal function ~S: ~S"
                 sym defs)
          ;; else
          defs))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GType and SCM class associations

(define %gtype-hash (make-hash-table 31))
(define %reverse-hash (make-hash-table 31))
(define %info-hash (make-hash-table 31))

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

(define (%gtype-dump-table)
  "Returns a list describing the current state of the GType to Scheme
class mapping. Each entry is the GType, the GType name, and the scheme
class."
  (hash-map->list
   (lambda (key val)
     (list key (gtype-get-name key) val))
   %gtype-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <GFundamental>

(define-class <GFundamental> ()
  (value #:class <scm-slot>
         #:init-keyword #:value
         #:init-value %null-pointer))

(define (%make-fundamental-class type dsupers ref unref)
  (make-class (cons <GFundamental> dsupers)
              `((ref #:allocation #:class
                     #:init-value ,ref)
                (unref #:allocation #:class
                       #:init-value ,unref))
              #:name type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boxed

(define-class <GBoxed> (<GFundamental>)
  (ref #:allocation #:each-subclass
       #:init-value %null-pointer)
  (unref #:allocation #:each-subclass
         #:init-value %null-pointer)
  (size #:allocation #:each-subclass
        #:init-value 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; signal

(define-class <signal> (<applicable-struct>)
  (name #:init-keyword #:name)
  (flags #:init-keyword #:flags
         #:init-value #f)
  (accumulator #:init-keyword #:accumulator
               #:init-value #f)
  (return-type #:init-keyword #:return-type
               #:init-value 0)
  (param-types #:init-keyword #:param-types
               #:init-value '())
  (output-mask #:init-keyword #:output-mask
               #:init-value #f))

(define make-signal (cute make <signal> <...>))

(define-method (initialize (signal <signal>) initargs)
  (next-method)
  (slot-set! signal 'procedure (cut %emit <> signal <...>)))

(define (%find-signal signal type)
  (let* ((%signals (filter (compose (cute is-a? <> <signal>) method-procedure)
                           (generic-function-methods signal)))
         (cpl (class-precedence-list type))
         (signals (filter (compose (cute memq <> cpl) car method-specializers)
                          %signals)))
    (and (not (null? signals))
         (method-procedure
          (car (sort signals
                     (lambda (a b)
                       (let ((a-type (car (method-specializers a)))
                             (b-type (car (method-specializers b))))
                         (let lp ((cpl (class-precedence-list type)))
                           (let ((elt (car cpl)))
                             (cond
                              ((eq? a-type elt) #t)
                              ((eq? b-type elt) #f)
                              (else (lp (cdr cpl))))))))))))))

(define* (connect-1 obj signal handler #:key after? detail)
  (let ((real-signal (if (is-a? signal <signal>)
                         signal
                         (%find-signal signal (class-of obj)))))
    (if real-signal
        (%connect obj real-signal detail handler after?)
        (error "~S has no signal in ~S" obj signal))))

(define-method (connect obj (signal <generic>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler rest))

(define-method (connect obj (signal <generic>) (detail <symbol>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler #:detail detail rest))

(define-method (connect obj (signal <signal>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler rest))

(define-method (connect obj (signal <signal>) (detail <symbol>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler #:detail detail rest))

(define-method (connect-after obj (signal <generic>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler #:after? #t rest))

(define-method (connect-after obj (signal <generic>) (detail <symbol>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler #:after? #t #:detail detail rest))

(define-method (connect-after obj (signal <signal>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler #:after? #t rest))

(define-method (connect-after obj (signal <signal>) (detail <symbol>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler #:after? #t #:detail detail rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(define-class <GInterface> (<GFundamental>)
  (ref #:allocation #:each-subclass
       #:init-value %null-pointer)
  (unref #:allocation #:each-subclass
         #:init-value %null-pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object

;; A bit of indirection since we can't know the pointer to
;; ref/unref functions until GLib/GObject is loaded
(define %gobject-ref %null-pointer)
(define %gobject-unref %null-pointer)

(define-class <GObject> (<GFundamental>)
  (ref #:allocation #:each-subclass
       #:init-value (lambda () %gobject-ref))
  (unref #:allocation #:each-subclass
         #:init-value (lambda () %gobject-unref)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variant

(define %gvariant-ref %null-pointer)
(define %gvariant-unref %null-pointer)

(define-class <GVariant> (<GFundamental>)
  (ref #:allocation #:each-subclass
       #:init-value (lambda () %gvariant-ref))
  (unref #:allocation #:each-subclass
         #:init-value (lambda () %gvariant-unref)))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Values and Params

(define %gvalue-ref %null-pointer)
(define %gvalue-unref %null-pointer)

(define-class <GValue> (<GBoxed> <applicable-struct-with-setter>)
  (size #:allocation #:each-subclass
        #:init-value SIZEOF_GVALUE)
  (ref #:allocation #:each-subclass
       #:init-value (lambda () %gvalue-ref))
  (unref #:allocation #:each-subclass
         #:init-value (lambda () %gvalue-unref)))

(define-method (initialize (value <GValue>) initargs)
  (next-method)
  (slot-set! value 'procedure (cut %get value))
  (slot-set! value 'setter
             (case-lambda
               ((v) (%set! value v))
               ((t v) (%set-type! value t) (%set! value v)))))

(define-method (transform (value <GValue>) gtype)
  (%transform value gtype))

(define %gparam-ref %null-pointer)
(define %gparam-unref %null-pointer)
(define-class <GParam> (<GFundamental> <applicable-struct-with-setter>)
  (ref #:allocation #:each-subclass
       #:init-value (lambda () %gparam-ref))
  (unref #:allocation #:each-subclass
         #:init-value (lambda () %gparam-unref)))

(define-method (initialize (pspec <GParam>) initargs)
  (next-method)
  (slot-set! pspec 'procedure (cut %get-property <> pspec))
  (slot-set! pspec 'setter (cut %set-property! <> pspec <>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Closures

(define %gclosure-ref %null-pointer)
(define %gclosure-unref %null-pointer)

(define-class <GClosure> (<GBoxed> <applicable-struct>)
  (size #:allocation #:each-subclass
        #:init-value SIZEOF_GCLOSURE)
  (ref #:allocation #:each-subclass
       #:init-value (lambda () %gclosure-ref))
  (unref #:allocation #:each-subclass
         #:init-value (lambda () %gclosure-unref)))

(define-method (initialize (closure <GClosure>) initargs)
  (next-method)
  (slot-set! closure 'procedure
             (match-lambda*
               (((type . out-mask) args ...)
                (%invoke-closure closure type out-mask args))
               ((type args ...)
                (%invoke-closure closure type #f args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enums

(define-class <GEnum> (<GFundamental>)
  (obarray #:allocation #:each-subclass
           #:init-value '()))

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

;;; Flags

(define-class <GFlags> (<GFundamental>)
  (obarray #:allocation #:each-subclass
           #:init-value '()))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-stage2)
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

  (type-associate G_TYPE_BOXED <GBoxed>)
  (type-associate G_TYPE_ENUM <GEnum>)
  (type-associate G_TYPE_FLAGS <GFlags>)
  (type-associate G_TYPE_OBJECT <GObject>)
  (type-associate G_TYPE_INTERFACE <GInterface>)
  (type-associate G_TYPE_PARAM <GParam>)
  (type-associate G_TYPE_VARIANT <GVariant>)
  )
