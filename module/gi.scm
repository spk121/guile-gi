;; Copyright (C), 2018, 2019 Michael L. Gran

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
(define-module (gi)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (gi core generics)
  #:use-module (gi core hooks)
  #:use-module (gi types)
  #:use-module (gi repository)
  #:re-export (<signal>
               make-signal
               connect
               connect-after
               ;; re-export some GOOPS stuff, so that we don't have to import all of it
               is-a?
               define-method
               ;; core-generics
               connect command-line equal? format write load send send shutdown quit
               ;; types
               ;; G_TYPE_NONE
               ;; G_TYPE_CHAR G_TYPE_UCHAR
               ;; G_TYPE_BOOLEAN
               ;; G_TYPE_INT G_TYPE_UINT
               ;; G_TYPE_INT64 G_TYPE_UINT64
               ;; G_TYPE_ENUM G_TYPE_FLAGS
               ;; G_TYPE_FLOAT G_TYPE_DOUBLE
               ;; G_TYPE_OBJECT G_TYPE_STRING
               ;; G_TYPE_POINTER
               <string>
               <GObject> <GInterface> <GParam> <GBoxed>
               <GVariant> <GValue> <GClosure>
               enum->number flags->number
               transform procedure->closure
               ;; from (gi core hooks)
               %before-function-hook
               %before-callback-hook
               %before-c-callback-hook
               )
  #:replace ((%new . make))
  #:export (use-typelibs
            register-type
))

(define (subclass? type-a type-b)
  (memq type-b (class-precedence-list type-a)))

(define (%typelib-module-name lib version)
  (list 'gi (string->symbol (string-append lib "-" version))))

(define (%gi->module-use form subform lib version params)
  (cond
   ((not (string? (syntax->datum lib)))
    (syntax-violation #f "unexpected library (not a string)"
                      form subform))
   ((not (string? (syntax->datum version)))
    (syntax-violation #f "unexpected version (not a string)"
                      form subform))
   (else
    (let ((module (datum->syntax form (%typelib-module-name (syntax->datum lib)
                                                            (syntax->datum version)))))
      #`(#,module #,@params)))))

(define (%gi->module-def form subform lib version)
  (cond
   ((not (string? (syntax->datum lib)))
    (syntax-violation #f "unexpected library (not a string)"
                      form subform))
   ((not (string? (syntax->datum version)))
    (syntax-violation #f "unexpected version (not a string)"
                      form subform))
   (else
    (let ((module (datum->syntax form (%typelib-module-name (syntax->datum lib)
                                                            (syntax->datum version)))))

      #`(unless (resolve-module '#,module #:ensure #f)
          (typelib->module '#,module #,lib #,version))))))

(define-syntax use-typelibs
  (lambda (x)
    (syntax-case x ()
      ((_ lib ...)
       (let ((module-defs
              (map
               (lambda (lib)
                 (syntax-case lib ()
                   ((typelib version)
                    (%gi->module-def x lib #'typelib #'version))
                   (((typelib version) _ ...)
                    (%gi->module-def x lib #'typelib #'version))))
               #'(lib ...)))
             (module-uses
              (map
               (lambda (lib)
                 (syntax-case lib ()
                   ((typelib version)
                    (%gi->module-use x lib #'typelib #'version '()))
                   (((typelib version) param ...)
                    (%gi->module-use x lib #'typelib #'version #'(param ...)))))
               #'(lib ...))))
         #`(eval-when (expand load eval)
             #,@module-defs
             (use-modules #,@module-uses)))))))

(load-extension "libguile-gi" "init_core_goops")
(load-extension "libguile-gi" "gig_init")


(define* (make-gobject gtype #:optional prop-list)
  (%make-gobject gtype prop-list))

(define (%new type . rest)
  (cond
   ((subclass? type <GObject>)
    (make-gobject type rest))
   ((subclass? type <GBoxed>)
    (%allocate-boxed type))
   ((subclass? type <GEnum>)
    (error "use symbol->enum or number->enum instead"))
   ((subclass? type <GFlags>)
    (error "use list->flags or number->flags instead"))
   (else
    (apply make type rest))))

(define (register-type name parent . rest)
  (cond
   ((subclass? parent <GObject>)
    (apply %define-object-type name parent rest))
   (else
    (error "cannot define class with parent ~A" parent))))

(when (defined? 'gcov-reset)
  (export gcov-reset))
(when (defined? 'gcov-dump)
  (export gcov-dump))
