(define-module (gi)
  ;; #:use-module (gi _gi)
  #:use-module (ice-9 eval-string)
  #:export (
	    get-typelib-search-path
	    prepend-typelib-search-path
	    load-typelib
	    import-typelib
	    export-typelib
	    gi-constant-value
	    gi-flag-value
	    gi-enum-value
	    gi-struct-ref
	    gi-struct-set
	    gi-function-invoke
	    gi-method-prepare
	    gi-method-send
	    gi-lookup-type
	    gi-lookup-callback-info
	    ))

(define (import-typelib namespace version)
  (eval-string (%import-typelib namespace version)))

(load-extension "libguile-gi.so" "gir_init")
