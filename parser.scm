(define *s-types* '())
(define *ils* '())

(define (load-info info flags)
  (define (is-registered x)
    (member x *s-types*))
  (define (register x)
    (set! *s-types* (cons x *s-types*)))
  
  (let ((parent (bases-info-get-container info))
	(parent-gtype (if parent
			  (registered-type-info-get-g-type parent)
			  G_TYPE_INVALID))
	(parent-name (if parent
			 (base-info-get-name parent)
			 #f))
	(t (base-info-get-type info)))
    (cond
     ((= t INFO_TYPE_CALLBACK)
      (format #t "~A - not loading ~A type because callbacks are not supported"
	      (base-info-get-name info) (info-type-to-string t)))
     ((or (= t INFO_TYPE_FUNCTION)
	  (= t INFO_TYPE_SIGNAL))
      (let* ((types #f)
	     (len 0)
	     (args-ok #t)
	     (namespace (base-info-get-namespace info))
	     (long-name (if parent-name
			    (set! long-name (callable-info-make-name info parent-name))
			    (set! long-name (callable-info-make-name info namespace))))
	     (amap (callable-info-make-amap info long-name))
	     (types (amap-get-gtype-
	
	     
	    
