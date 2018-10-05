(define-module (gi _none)
  #:use-module (oop goops)
  #:export ($NONE
	    none?))

;; $NONE is a value used to indicate a missing parameter.

(define-class <NONE> ()
  (_x #:init-value 0))

(define $NONE (make <NONE>))

(define (none? x)
  (eq? x $NONE))
	    
