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
  #:export (send
            connect
            use-typelibs))

(eval-when (expand load eval)
  ;; required for %typelib-module-name, which is used at expand time
  (load-extension "libguile-gi" "gir_init_typelib_private"))

;; This macro derives from
;; https:;;lists.gnu.org/archive/html/guile-user/2018-12/msg00037.html

(define-syntax send
  (lambda (stx)
    (syntax-case stx ()
      ((_ self (method arg ...))
       (identifier? #'method)
       (with-syntax ((method-str (symbol->string
                                  (syntax->datum #'method))))
         #'(call-method self method-str arg ...))))))

(define-syntax connect
  (lambda (stx)
    (syntax-case stx ()
      ((_ self (signal handler))
       (identifier? #'signal)
       (with-syntax ((signal-str (symbol->string
                                  (syntax->datum #'signal))))
         #'(signal-connect self signal-str handler))))))

(define (%gi->module-use x lib version params)
  (cond
   ((not (string? (syntax->datum lib)))
    #`(syntax-error "expected string but got " lib))
   ((not (string? (syntax->datum version)))
    #`(syntax-error "expected string but got " version))
   (else
    (let ((module (datum->syntax x (%typelib-module-name (syntax->datum lib)
                                                         (syntax->datum version)))))
      #`(#,module #,@params)))))

(define (%gi->module-def x lib version)
  (cond
   ((not (string? (syntax->datum lib)))
    #`(syntax-error "expected string but got" lib))
   ((not (string? (syntax->datum version)))
    #`(syntax-error "expected string but got" version))
   (else
    (let ((module (datum->syntax x (%typelib-module-name (syntax->datum lib)
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
                    (%gi->module-def x #'typelib #'version))
                   (((typelib version) _ ...)
                    (%gi->module-def x #'typelib #'version))))
               #'(lib ...)))
             (module-uses
              (map
               (lambda (lib)
                 (syntax-case lib ()
                   ((typelib version)
                    (%gi->module-use x #'typelib #'version '()))
                   (((typelib version) param ...)
                    (%gi->module-use x #'typelib #'version #'(param ...)))))
               #'(lib ...))))
         #`(eval-when (expand load eval)
             #,@module-defs
             (use-modules #,@module-uses)))))))

(load-extension "libguile-gi" "gir_init")

(when (defined? 'gcov-reset)
  (export gcov-reset))
(when (defined? 'gcov-dump)
  (export gcov-dump))
