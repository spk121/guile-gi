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
;; along with this program.  If not, see <https:;;www.gnu.org/licenses/>.

(define-module (gi oop)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:export (<GObject>
            <GBoxed>
            <GParamSpec>
            <signal>
            register-type

            connect-after)
  #:replace (connect))

(eval-when (expand load eval)
  (load-extension "libguile-gi" "gig_init_object"))

(define-class <GObject> ()
  (object #:class <scm-slot>
          #:init-keyword #:object
          #:init-value %null-pointer))

(define-class <GInterface> ()
  (object #:class <scm-slot>
          #:init-keyword #:object
          #:init-value %null-pointer))

(define-class <GBoxed> ()
  (value #:class <scm-slot>
         #:init-keyword #:value
         #:init-value %null-pointer))

(define-class <GCompact> ()
  (ptr #:class <scm-slot>
       #:init-keyword #:ptr
       #:init-value %null-pointer)
  (ref #:class <foreign-slot>
       #:allocation #:each-subclass
       #:init-value %null-pointer)
  (unref #:class <foreign-slot>
         #:allocation #:each-subclass
         #:init-value %null-pointer))

(define-class <GParamSpec> (<applicable-struct-with-setter>)
  (pspec #:class <scm-slot>
         #:init-keyword #:pspec
         #:init-value %null-pointer))

(define (%make-gobject type object)
  (make type #:object object))

(define (%make-boxed type value)
  (make type #:value value))

(define (%make-paramspec type pspec)
  (make type #:pspec pspec))

(define (%make-compact type ptr)
  (make type #:ptr ptr))

(define-method (initialize (pspec <GParamSpec>) initargs)
  (next-method)
  (slot-set! pspec 'procedure (cut %get-property <> pspec))
  (slot-set! pspec 'setter (cut %set-property! <> pspec <>)))

(define-class <signal> (<applicable-struct>)
  (name #:init-keyword #:name)
  (flags #:init-keyword #:flags
         #:init-value 0)
  (accumulator #:init-keyword #:accumulator
               #:init-value #f)
  (return-type #:init-keyword #:return-type
               #:init-value 0)
  (param-types #:init-keyword #:param-types
               #:init-value '()))

(define-method (initialize (signal <signal>) initargs)
  (next-method)
  (slot-set! signal 'procedure (cut %emit <> signal <...>)))

(define-method (connect (socket <input-output-port>) . args)
  (apply (@ (guile) connect) socket args))

(define-method (connect obj (signal <signal>) (handler <procedure>))
  (%connect obj signal #f handler))

(define-method (connect obj (signal <signal>) (detail <symbol>) (handler <procedure>))
  (%connect obj signal detail handler))

(define-method (connect-after obj (signal <signal>) (handler <procedure>))
  (%connect obj signal #f handler #t))

(define-method (connect-after obj (signal <signal>) (detail <symbol>) (handler <procedure>))
  (%connect obj signal detail handler #t))

(define (register-type name parent . rest)
  (cond
   ((memq <GObject> (class-precedence-list parent))
    (apply %define-object-type name parent rest))
   (else
    (error "cannot define class with parent ~A" parent))))
