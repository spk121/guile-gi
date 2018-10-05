(define-module (gir _info)
  #:use-module (oop goops)
  #:use-module (system foreign-object)
  #:export())


;;(define-class gi.BaseInfo ()
;;  (data #:init-thunk (lambda () (make-pointer 0)) #:getter get-pointer #:setter set-pointer!))

(define (construct-base-info x)
  #t)

(define (finalize-base-info x)
  #t)

(define-foreign-object-type
  gi.BaseInfo
  construct-base-info
  (info weakreflist cache)
  #:finalizer finalize-base-info)

(define-method (get-type (self gi.BaseInfo))
  (%base-info-get-type self))

(define-method (get-type (self gi.BaseInfo))
  (%base-info-get-name self))

(define-method (get-name-unescaped (self gi.BaseInfo))
  (%base-info-get-name-unescaped self))

(define-method (get-namespace (self gi.BaseInfo))
  (%base-info-get-namespace self))

(define-method (is-deprecated? (self gi.BaseInfo))
  (%base-info-is-deprecated? self))

(define-method (get-attribute (self gi.BaseInfo) attribute)
  (%base-info-get-attribute self attribute))

(define-method (get-container (self gi.BaseInfo))
  (%base-info-get-container self))

(define-method (equal? (self gi.BaseInfo) (other gi.BaseInfo))
  (%base-info-equal? self other))

(define-class gi.CallableInfo (gi.BaseInfo))

(define-method (invoke (self gi.CallableInfo) . args)
  (%callable-info-invoke self args))

(define-method (get-arguments (self gi.CallableInfo))
  (%callable-info-get-arguments self))

(define-method (get-return-type (self gi.CallableInfo))
  (%callable-info-get-return-type self))

(define-method (may-return-null? (self gi.CallableInfo))
  (%callable-info-may-return-null? self))

(define-method (skip-return (self gi.CallableInfo))
  (%callable-info-skip-return? self))

(define-method (get-return-attribute (self gi.CallableInfo))
  (%callable-info-get-return-attribute self))

(define-method (can-throw-gerror? (self gi.CallableInfo))
  (%callable-info-can-throw-gerror? self))

(define-class gi.CallbackInfo (gi.CallableInfo))

(define-class gi.ErrorDomainInfo (gi.BaseInfo))

(define-class gi.SignalInfo (gi.CallableInfo))

(define-method (get-flags (self gi.SignalInfo))
  (%signal-info-get-flags self))

(define-method (get-class-closure (self gi.SignalInfo))
  (%signal-info-get-class-closure self))

(define-method (true-stops-emit? (self gi.SignalInfo))
  (%signal-info-true-stops-emis? self))

(define-class gi.PropertyInfo (gi.BaseInfo))

(define-method (get-flags (self gi.PropertyInfo))
  (%property-info-get-flags self))

(define-method (get-type (self gi.PropertyInfo))
  (%property-info-get-type self))
  
(define-method (get-ownership-transfer (self gi.PropertyInfo))
  (%property-info-get-ownership-transfer self))

(define-class gi.ArgInfo (gi.BaseInfo))

(define-method (get-direction (self gi.ArgInfo))
  (%arg-info-get-direction self))

(define-method (is-caller-allocates? (self gi.ArgInfo))
  (%arg-info-is-caller-allocates? self))

(load-extension ".libs/libguile-gi.so" "gir_info_register_types")
