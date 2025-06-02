
;; Functions to convert typelib info to IL
(define-module (gi typelib)
  #:use-module (gi girepository)
  #:export (info->IL-constant))

(define (info->IL info)
  #f)

(define (info->IL-constant info)
  (let* ((name (base-info-get-name info))
	 (typeinfo (constant-info-get-type info))
	 (typetag (type-info-get-tag typeinfo))
	 (value (constant-info-get-value info)))
    (list '^constant name value)))
