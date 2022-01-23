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
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module ((gi core generics) #:select (connect))
  #:use-module (gi core objects)
  #:use-module (gi core signals)
  #:use-module (gi core flags-and-enums)
  #:export (initialize
            transform
            connect-after
            procedure->closure
            make-gobject
            %allocate-boxed
            %define-object-type)
  #:re-export (;; from core generics
               connect
               ;; from core objects
               <GBoxed>
               <GObject>
               <GInterface>
               <GParam>
               <GVariant>
               <GClosure>
               <GValue>
               ;; from core signals
               <signal>
               make-signal
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
  (load-extension "libguile-gi" "init_core_goops")
  (load-extension "libguile-gi" "gig_init_object")
  (load-extension "libguile-gi" "gig_init_types")
  (load-extension "libguile-gi" "gig_init_closure"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects

(define* (make-gobject gtype #:optional prop-list)
  (%make-gobject gtype prop-list))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signals

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
  (slot-set! pspec 'procedure (cut %get-property <> pspec))
  (slot-set! pspec 'setter (cut %set-property! <> pspec <>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Closures

(define-method (initialize (closure <GClosure>) initargs)
  (next-method)
  #;(slot-set! closure 'procedure
             (match-lambda*
               (((type . out-mask) args ...)
                (%invoke-closure closure type out-mask args))
               ((type args ...)
                (%invoke-closure closure type #f args))))
  )

