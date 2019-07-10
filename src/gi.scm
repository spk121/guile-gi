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
;; along with this program.  If not, see <https:;;www.gnu.org/licenses/>.
(define-module (gi)
  #:use-module (gi oop)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:re-export (<signal>
               make-signal
               connect
               connect-after
               ;; re-export some GOOPS stuff, so that we don't have to import all of it
               is-a?
               define-method)
  #:export (use-typelibs
            create
            (%create . make-gobject)
            with-object
            (with-object . using)
            register-type))

(eval-when (expand load eval)
  ;; required for %typelib-module-name, which is used at expand time
  (load-extension "libguile-gi" "gir_init_typelib_private"))

(define %create (@@ (gi oop) %create))
(define %syntax->string (compose symbol->string syntax->datum))

(define-syntax create
  (lambda (stx)
    (syntax-case stx ()
      ((_ type field ...)
       #`(%create type
                  `#,(map (lambda (field)
                            (syntax-case field ()
                              ((key val)
                               (identifier? #'key)
                               (with-syntax ((key-str (%syntax->string #'key)))
                                 #'(key-str . ,val)))))
                          #'(field ...)))))))

;; temporaries while property objects are not yet everywhere
(define-public (gobject-get-property obj prop)
  (((@@ (gi oop) %object-get-pspec) obj prop) obj))

(define-public (gobject-set-property! obj prop val)
  (set! (((@@ (gi oop) %object-get-pspec) obj prop) obj) val))

(define-syntax with-object
  (lambda (stx)
    (syntax-case stx ()
      ((_ self block ...)
       (identifier? #'self)
       #`(begin
           #,@(map
               (lambda (block)
                 (syntax-case block (set!
                                     connect! connect-after!
                                     remove! block! unblock!)
                   ;; properties
                   (prop
                    (identifier? #'prop)
                    (with-syntax ((prop-str (%syntax->string #'prop)))
                      #'(gobject-get-property self prop-str)))
                   ((set! prop val)
                    (identifier? #'prop)
                    (with-syntax ((prop-str (%syntax->string #'prop)))
                      #'(gobject-set-property! self prop-str val)))
                   ;; signals
                   ((connect! signal handler)
                    (identifier? #'signal)
                    (with-syntax ((name (%syntax->string #'signal)))
                      #'(connect self (make <signal> #:name name) handler)))
                   ((connect-after! signal handler)
                    (identifier? #'signal)
                    (with-syntax ((name (%syntax->string #'signal)))
                      #'(connect-after self (make <signal> #:name name) handler)))
                   ((remove! handler)
                    (syntax-violation #f "remove! is no longer supported"
                                      with-object block))
                   ((block! handler)
                    (syntax-violation #f "block! is no longer supported"
                                      with-object block))
                   ((unblock! handler)
                    (syntax-violation #f "unblock! is no longer supported"
                                      with-object block))
                   ;; methods
                   ((method arg ...)
                    (identifier? #'id)
                    (with-syntax ((method-str (%syntax->string #'method)))
                      #'(call-method self method-str arg ...)))))
               #'(block ...))))
      ((_ self block ...)
       #'(let ((this self)) (with-object this block ...))))))

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
          (%typelib-define-module #,lib #,version))))))

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

(load-extension "libguile-gi" "gir_init")

(define-method (initialize (pspec <GParam>) initargs)
  (next-method)
  (slot-set! pspec 'procedure (cut (@@ (gi oop) %get-property) <> pspec))
  (slot-set! pspec 'setter (cut (@@ (gi oop) %set-property!) <> pspec <>)))

(define (register-type name parent . rest)
  (cond
   ((memq <GObject> (class-precedence-list parent))
    (apply (@@ (gi oop) %define-object-type) name parent rest))
   (else
    (error "cannot define class with parent ~A" parent))))

(when (defined? 'gcov-reset)
  (export gcov-reset))
(when (defined? 'gcov-dump)
  (export gcov-dump))
