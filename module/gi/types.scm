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
  #:use-module ((gi core generics) #:select (connect))
  #:use-module (gi core fundamental)
  #:use-module (gi core objects)
  #:use-module (gi core flags-and-enums)
  #:export (initialize
            transform
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
            )
  #:re-export (;; from core generics
               connect
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
               ))

(eval-when (expand load eval)
  (load-extension "libguile-gi-types" "gig_init_types")
)

;; This pair of hash tables bidirectionally maps GType integers to SCM
;; classes that are usually subclasses of <GFundamental>
(define $gtype-scm-hash (make-hash-table 100))
(define $scm-gtype-hash (make-hash-table 100))

;; This hash table holds only those SCM types that don't have a GType,

;; like some enum and flag types.
(define $name-scm-hash (make-hash-table 4))

(define $log-port (current-output-port))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define (type-register-self gtype stype)
  (let ((pval (hash-ref $gtype-scm-hash gtype)))
    (unless pval
      (hash-set! $gtype-scm-store gtype stype)
      (format $log-port "~A - registering a new type for ~A as ~S~%"
              (gtype-get-name gtype) gtype stype))))

(define (type-register gtype stype)
  (let ((parent (gtype-get-parent gtype)))
    (unless parent
      (type-register-self parent #f))
    (type-register-self gtype stype)))

(define (type-associate gtype stype)
  "Sets up the two-way association between the GType integer and its
SCM class.  It returns the class name."
  (type-register-self gtype stype)
  (set-object-property! stype 'sort-key (hash-count (const #t) $gtype-scm-hash))
  (hash-set! $scm-gtype-store stype gtype)
  (class-name stype))


#|
(define %object-ref-sink #f)
(define %object-unref #f)
(define %param-spec-ref-sink #f)
(define %param-spec-unref #f)
(define %variant-ref-sink #f)
(define %variant-unref #f)
(define %make-gobject #f)
(define %emit #f)                       ; move to %signal-emit
(define %connect #f)                    ; %signal-connect
(define %get #F)                        ; %value-get
(define %set! #f)                       ; %value-set!
(define %set-type! #f)                  ; %value-type-set!
(define %transform #f)                  ; %value-transform
(define %get-property #F)
(define %set-property! #f)
(define %invoke-closure #f)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects

(define-class <GObject> (<GFundamental>)
  (ref #:allocation #:each-subclass
       #:init-value $ref-sink-object)
  (unref #:allocation #:each-subclass
         #:init-value $unref-object))

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
;;; Values

;; Has 'procedure and 'setter slots
(define-class <GValue> (<GFundamental> <applicable-struct-with-setter>)
  (ref #:allocation #:each-subclass
       #:init-value %null-pointer)
  (unref #:allocation #:each-subclass
         #:init-value %null-pointer))

(define-method (initialize (value <GValue>) initargs)
  (next-method)
  (slot-set! value 'procedure (cut %get value))
  (slot-set! value 'setter
             (case-lambda
              ((v) (%set! value v))
              ((t v) (%set-type! value t) (%set! value v)))))

(define-method (transform (value <GValue>) gtype)
  (%transform value gtype))

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
  (slot-set! pspec 'procedure (cut %get-property <> pspec))
  (slot-set! pspec 'setter (cut %set-property! <> pspec <>)))

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
                (%invoke-closure closure type out-mask args))
               ((type args ...)
                (%invoke-closure closure type #f args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variants

(define-class <GVariant> (<GFundamental>)
  (ref #:allocation #:each-subclass
       #:init-value $ref-sink-variant)
  (unref #:allocation #:each-subclass
         #:init-value $unref-variant))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize)
  
  ($type-register $G_TYPE_INT <integer>)
  ($type-register $G_TYPE_UINT <integer>)
  ($type-register $G_TYPE_LONG <integer>)
  ($type-register $G_TYPE_ULONG <integer>)
  ($type-register $G_TYPE_INT64 <integer>)
  ($type-register $G_TYPE_UINT64 <integer>)
  ($type-register $G_TYPE_FLOAT <real>)
  ($type-register $G_TYPE_DOUBLE <real>)
  ;; ($type-register $G_TYPE_HASH_TABLE <hashtable>)

  ($type-associate $G_TYPE_STRING <string>)

  ($save-fundamental-type <GFundamental>)
  ($save-boxed-type ($type-associate $G_TYPE_BOXED <GBoxed>))
  ($save-object-type ($type-associate $G_TYPE_OBJECT <GObject>))
  ($save-enum-type ($type-associate $G_TYPE_ENUM <GEnum>))
  ($save-flags-type ($type-associate $G_TYPE_FLAGS <GFlags>))
  ($save-interface-type ($type-associate $G_TYPE_INTERFACE <GInterface>))
  ($save-paramspec-type ($type-associate $G_TYPE_PARAM <GParam>))
  ($save-variant-type ($type-associate $G_TYPE_VARIANT <GVariant>))
  ($save-value-type ($type-associate $G_TYPE_VALUE <GValue>))
  ($save-closure-type ($type-associate $G_TYPE_CLOSURE <GClosure>)))


(initialize)
