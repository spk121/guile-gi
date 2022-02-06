;; Copyright (C) 2019, 2022 Michael L. Gran

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
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (gi core fundamental)
  #:use-module (gi core objects)
  #:use-module (gi core flags-and-enums)
  #:export (initialize
            transform
            connect-before
            connect-after
            procedure->closure
            make-gobject
            %define-object-type
            <GObject>
            <GParam>
            <GVariant>
            <GClosure>
            <GValue>
               
            <signal>
            make-signal

            G_TYPE_NONE
            G_TYPE_BOOLEAN
            G_TYPE_CHAR
            G_TYPE_UCHAR
            G_TYPE_INT
            G_TYPE_UINT
            G_TYPE_LONG
            G_TYPE_ULONG
            G_TYPE_INT64
            G_TYPE_UINT64
            G_TYPE_FLOAT
            G_TYPE_DOUBLE
            G_TYPE_ENUM
            G_TYPE_FLAGS
            G_TYPE_OBJECT
            G_TYPE_STRING
            G_TYPE_POINTER

            gtype-get-name
            allocate-boxed
            )
  #:re-export (
               ;; from core objects
               <GBoxed>
               <GInterface>
               ;; from core flags-and-enums
               <GEnum> <GFlags>
               enum->number enum->symbol number->enum symbol->enum
               flags->number flags->list number->flags list->flags flags-set?
               enum-universe
               flags-mask flags-union flags-intersection flags-difference
               flags-complement flags-projection flags-projection/list
               flags-projection/number               
               )
  #:duplicates (merge-generics)
  #:declarative? #f
  )

(eval-when (expand load eval)
  (load-extension "libguile-gi" "init_core_oop")
  (load-extension "libguile-gi" "gig_init_types")
  (load-extension "libguile-gi" "gig_init_flag")
  (load-extension "libguile-gi" "gig_init_signal")
  (load-extension "libguile-gi" "gig_init_value")
  (load-extension "libguile-gi" "gig_init_object")
  (load-extension "libguile-gi" "gig_init_closure")
)


(define G_TYPE_INVALID $G_TYPE_INVALID)
(define G_TYPE_NONE $G_TYPE_NONE)
(define G_TYPE_CHAR $G_TYPE_CHAR)
(define G_TYPE_UCHAR $G_TYPE_UCHAR)
(define G_TYPE_BOOLEAN $G_TYPE_BOOLEAN)
(define G_TYPE_INT $G_TYPE_INT)
(define G_TYPE_UINT $G_TYPE_UINT)
(define G_TYPE_LONG $G_TYPE_LONG)
(define G_TYPE_ULONG $G_TYPE_LONG)
(define G_TYPE_INT64 $G_TYPE_INT64)
(define G_TYPE_UINT64 $G_TYPE_UINT64)
(define G_TYPE_ENUM $G_TYPE_ENUM)
(define G_TYPE_FLAGS $G_TYPE_FLAGS)
(define G_TYPE_FLOAT $G_TYPE_FLOAT)
(define G_TYPE_DOUBLE $G_TYPE_DOUBLE)
(define G_TYPE_GTYPE $G_TYPE_GTYPE)
(define G_TYPE_OBJECT $G_TYPE_OBJECT)
(define G_TYPE_STRING $G_TYPE_STRING)
(define G_TYPE_POINTER $G_TYPE_POINTER)

(define make-gobject $make-gobject)
  
(define (get-gtype x)
  "Returns the integer GType of the argument, which should either be a
class or an instance of a class.  Returns zero on failure: zero is the
value of G_TYPE_INVALID."
  ($get-gtype x))

(define (gtype-get-scheme-type type)
  "Given an integer GType, this returns Scheme class associated with
it, or #f if the GType is not associated with a Scheme class."
  ;; FIXME return #f
  ($gtype-get-scheme-type type))

(define (gtype-get-name type)
  "Given an integer GType, this returns a string which is the name of
the GType is according to its typelib file.  Note that this name is
different from the name of GType's associated Scheme class."
  ($gtype-get-name type))

(define (gtype-get-parent type)
  "Given an integer GType, this returns an integer GType that is this
type's parent."
  ($gtype-get-parent type))

(define (gtype-get-fundamental type)
  "Given an integer GType, this returns an integer GType that is this
type's fundamental type."
  ($gtype-get-fundamental type))

(define (gtype-get-children type)
  "Given an integer GType, this returns a list of integers that are
children type of this GType."
  ($gtype-get-children type))

(define (gtype-get-interfaces type)
  "Given an integer GType, this returns a list of integers that
are interface types of this GType."
  ($gtype-get-interfaces type))

(define (gtype-get-depth type)
  "Given an integer GType, this returns an integer which is the depth
of this GType in this GObject class structure."
  ($gtype-get-depth type))

(define (gtype-is-interface? type)
  "Given an integer GType, this returns #t if the GType is a GObject
interface type, and #f otherwise."
  ($gtype-is-interface? type))

(define (gtype-is-classed? type)
  "Given an integer GType, this returns #t if the GType is a GObject
classed type, and #f otherwise."
  ($gtype-is-classed? type))

(define (gtype-is-instantiatable? type)
  "Given an integer GType, this returns #t if the GType is a GObject
type that can be used to instantiate objects."
  ($gtype-is-instantiatable? type))

(define (gtype-is-derivable? type)
  "Given an integer GType, this returns #t if the GType is a GObject
type that can be a base class for another type, or #f otherwise."
  ($gtype-is-derivable? type))

(define (gtype-is-a? self parent)
  "Given two Guile integer GTypes, this returns #t if the second
is a parent to the first, or #f otherwise."
  ($gtype-is-a? self parent))

(define (gtype-alist)
  "This returns an association list of known GTypes and their
associated SCM GOOPS classes.  This list is output: modifying it has
not effect."
  ($gtype-alist))

(define (allocate-boxed type)
  "Given GOOPS class that is a subclass of <GBoxed>, this
creates an instance of that class where value slot is a pointer
to a zero-allocated area in memory which is the correct size
for this type."
  ($allocate-boxed type))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects


(define-class <GObject> (<GFundamental>)
  (ref #:allocation #:each-subclass
       #:init-value $ref-sink-object)
  (unref #:allocation #:each-subclass
         #:init-value $unref-object)
  (method #:allocation #:each-subclass
          #:init-value '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signals

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

(define-method (initialize (signal <signal>) initargs)
  (next-method)
  (slot-set! signal 'procedure (cut $emit-signal <> signal <...>)))

(define make-signal (cute make <signal> <...>))

(define (find-signal-methods generic-func)
  (filter
   (lambda (method)
     (is-a? (method-procedure method) <signal>))
   (generic-function-methods generic-func)))

(define (find-signals signal-methods precedence-list)
  (filter
   (lambda (method)
     (memq (car (method-specializers method))
           precedence-list))
   signal-methods))

(define (%find-signal generic-func type)
  (let* ((signal-methods (find-signal-methods generic-func))
         (cpl (class-precedence-list type))
         (signals (find-signals signal-methods cpl)))
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
        ($connect obj real-signal detail handler after?)
        (error "~S has no signal in ~S" obj signal))))

(define-method (connect-before obj (signal <generic>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler rest))

(define-method (connect-before obj (signal <generic>) (detail <symbol>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler #:detail detail rest))

(define-method (connect-before obj (signal <signal>) (handler <procedure>) . rest)
  (apply connect-1 obj signal handler rest))

(define-method (connect-before obj (signal <signal>) (detail <symbol>) (handler <procedure>) . rest)
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
;;; Values

;; Has 'procedure and 'setter slots
(define-class <GValue> (<applicable-struct-with-setter> <GFundamental> )
  (ref #:allocation #:each-subclass
       #:init-value %null-pointer)
  (unref #:allocation #:each-subclass
         #:init-value %null-pointer))

(define-method (initialize (self <GValue>) initargs)
  (next-method)
  (slot-set! self 'procedure (cut $get-value self))
  (slot-set! self 'setter
             (case-lambda
              ((v) ($set-value! self v))
              ((t v) ($set-value-type! self t) ($set-value! self v)))))

(define-method (transform (value <GValue>) gtype)
  ($transform-value value gtype))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Params

;; Has per-instance 'procedure and 'setter slots
(define-class <GParam> (<GFundamental> <applicable-struct-with-setter>)
  (ref #:allocation #:each-subclass
       #:init-value $ref-sink-param-spec)
  (unref #:allocation #:each-subclass
         #:init-value $unref-param-spec))

(define-method (initialize (pspec <GParam>) initargs)
  (next-method)
  (slot-set! pspec 'procedure (cut $get-property <> pspec))
  (slot-set! pspec 'setter (cut $set-property! <> pspec <>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Closures

;; Has a per-instance 'procedure slot
(define-class <GClosure> (<GFundamental> <applicable-struct>)
  (ref #:allocation #:each-subclass
       #:init-value $ref-sink-param-spec)
  (unref #:allocation #:each-subclass
         #:init-value $unref-param-spec))

(define-method (initialize (closure <GClosure>) initargs)
  (next-method)
  (slot-set! closure 'procedure
             (match-lambda*
               (((type . out-mask) args ...)
                ($invoke-closure closure type out-mask args))
               ((type args ...)
                ($invoke-closure closure type #f args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variants

(define-class <GVariant> (<GFundamental>)
  (ref #:allocation #:each-subclass
       #:init-value $ref-sink-variant)
  (unref #:allocation #:each-subclass
         #:init-value $unref-variant))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (_initialize)

  ($type-register $G_TYPE_CHAR <integer>)
  ($type-register $G_TYPE_UCHAR <integer>)
  ($type-register $G_TYPE_INT <integer>)
  ($type-register $G_TYPE_UINT <integer>)
  ($type-register $G_TYPE_LONG <integer>)
  ($type-register $G_TYPE_ULONG <integer>)
  ($type-register $G_TYPE_INT64 <integer>)
  ($type-register $G_TYPE_UINT64 <integer>)
  ($type-register $G_TYPE_FLOAT <real>)
  ($type-register $G_TYPE_DOUBLE <real>)
  ($type-register $G_TYPE_POINTER <foreign>)
  ;; ($type-register $G_TYPE_HASH_TABLE <hashtable>)

  ($type-register $G_TYPE_ARRAY <bytevector>)

  ($type-associate $G_TYPE_STRING <string>)
  ($type-associate $G_TYPE_BOOLEAN <boolean>)
  ;; ($type-associate $G_TYPE_BYTE_ARRAY <bytevector>)

  ($save-fundamental-type <GFundamental>)
  ($type-associate $G_TYPE_BOXED <GBoxed>)
  ($save-boxed-type <GBoxed>)
  ($type-associate $G_TYPE_OBJECT <GObject>)
  ($save-object-type <GObject>)
  ($type-associate $G_TYPE_ENUM <GEnum>)
  ($save-enum-type <GEnum>)
  ($type-associate $G_TYPE_FLAGS <GFlags>)
  ($save-flags-type <GFlags>)
  ($type-associate $G_TYPE_INTERFACE <GInterface>)
  ($save-interface-type <GInterface>)
  ($type-associate $G_TYPE_PARAM <GParam>)
  ($save-paramspec-type <GParam>)
  ($type-associate $G_TYPE_VARIANT <GVariant>)
  ($save-variant-type <GVariant>)

  ;; We can't associate these yet because they don't have constant
  ;; GType values.
  ($type-register-by-symbol '<GValue> <GValue>)
  ($save-value-type <GValue>)
  ($type-register-by-symbol '<GClosure> <GClosure>)
  ($save-closure-type <GClosure>)

  ($save-signal-type <signal>)
  ($save-make-signal-proc make-signal)
  )


(_initialize)
