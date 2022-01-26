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

;;(eval-when (expand load eval)
  ;;(load-extension "libguile-gi" "init_core_goops")
  ;;(load-extension "libguile-gi" "gig_init_object")
  ;;(load-extension "libguile-gi" "gig_init_types")
  ;;(load-extension "libguile-gi" "gig_init_closure")
;;  )

(define %object-ref-sink #f)
(define %object-unref #f)
(define %param-spec-ref-sink #f)
(define %param-spec-unref #f)
(define %variant-ref-sink #f)
(define %variant-unref #f)
(define %make-gobject #f)
(define %emit #f)
(define %connect #f)
(define %get #F)
(define %set! #f)
(define %set-type! #f)
(define %transform #f)
(define %get-property #F)
(define %set-property! #f)
(define %invoke-closure #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects

(define-class <GObject> (<GFundamental>)
  (ref #:allocation #:each-subclass
       #:init-value %object-ref-sink)
  (unref #:allocation #:each-subclass
         #:init-value %object-unref))

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
  (slot-set! signal 'procedure (cut %emit <> signal <...>)))

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
       #:init-value %param-spec-ref-sink)
  (unref #:allocation #:each-subclass
         #:init-value %param-spec-unref))

(define-method (initialize (pspec <GParam>) initargs)
  (next-method)
  (slot-set! pspec 'procedure (cut %get-property <> pspec))
  (slot-set! pspec 'setter (cut %set-property! <> pspec <>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Closures

;; Has a per-instance 'procedure slot
(define-class <GClosure> (<GFundamental> <applicable-struct>)
  (ref #:allocation #:each-subclass
       #:init-value %param-spec-ref-sink)
  (unref #:allocation #:each-subclass
         #:init-value %param-spec-unref))

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
       #:init-value %variant-ref-sink)
  (unref #:allocation #:each-subclass
         #:init-value %variant-unref))
