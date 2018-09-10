(define-module (gir _repository)
  #:use-module (oop goops)
  #:use-module (system foreign-object)
  ;; #:use-module (gir _ir)
  #:export (
	    make-repository
	    enumerate-versions
	    require-namespace
	    is-registered?
	    find-by-name
	    get-infos
	    get-typelib-path
	    get-version
	    get-loaded-namespaces
	    get-dependencies
	    get-immediate-dependencies
	    ))

;; (define-foreign-object-type
;;   gi.Repository
;;   make-empty-repository
;;   (repository))

;;   #:finalizer finalize-repository)

(load-extension "libguile-gir.so" "gir_module_init")

(define (make-repository)
  (%irepository-get-default))

(define-method (enumerate-versions (self gi.Repository) (namespace <string>))
  (%irepository-enumerate-versions self namespace))

(define-method (get-default (self gi.Repository))
  (%irepository-get-default self))

(define-method (require-namespace (self gi.Repository) (namespace <string>))
  (display 2)
  (%irepository-require self namespace))

(define-method (require-namespace (self gi.Repository) (namespace <string>) (version <string>))
  (display 3)
  (%irepository-require self namespace version))

(define-method (require-namespace (self gi.Repository) (namespace <string>) (version <string>) lazy)
  (display 4)
  (%irepository-require self namespace version lazy))

(define-method (is-registered? (self gi.Repository) (namespace <string>))
  (%irepository-is-registered? self namespace))

(define-method (is-registered? (self gi.Repository) (namespace <string>) (version <string>))
  (%irepository-is-registered? self namespace version))

(define-method (find-by-name (self gi.Repository) (namespace <string>) (name <string>))
  (%irepository-find-by-name self namespace name))

(define-method (get-infos (self gi.Repository))
  (%irepository-get-infos self))

(define-method (get-typelib-path (self gi.Repository) (namespace <string>))
  (%irepository-get-typelib-path self namespace))

(define-method (get-version (self gi.Repository) (namespace <string>))
  (%irepository-get-version self namespace))

(define-method (get-loaded-namespaces (self gi.Repository))
  (%irepository-get-loaded-namespaces self))

(define-method (get-dependencies (self gi.Repository) (namespace <string>))
  (%irepository-get-dependencies self namespace))

(define-method (get-immediate-dependencies (self gi.Repository) (namespace <string>))
  (%irepository-get-immediate-dependencies self namespace))

