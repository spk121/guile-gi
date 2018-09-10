(define-module (gi)
  #:use-module (gi _gi)
  #:export (
	    require-version
	    ;; require-foreign
	    ;; check-version
	    ;; get-required-version
	    ;; version-info
	    ;; import
	    ))

;; An association-list of namespaces and versions
;; e.g. (("Gtk" . "3.0"))
(define _versions '())

(define (require-version namespace version)
  "Ensures the correct versions are loaded when importing 'gi' modules.
NAMESPACE - a string - The name of the module to require
VERSION - a string - The version number of the module to require

If the module/version is already loaded, already required or unavailable,
it will throw an error.

Example:
  (use-module gi)
  (require-version \"Gtk\" \"3.0\")
"
  (assert-string namespace)
  (assert-string version)
  (let ((repository (get-default Repository)))
    (when (member namespace (get-loaded-namespaces repository))
      (let ((loaded-version (get-version namespace)))
	(unless (equal? loaded-version version)
	  (scm-error 'misc-error "require-version"
		     "Namespace ~A is already loaded with version ~A"
		     (list namespace loaded-version)
		     #f)))))

  (let ((required-version (assoc-ref _versions namespace)))
    (when (and required-verion
	       (not (equal? verion required-version)))
      (scm-error 'misc-error "require-version"
		 "Namespace ~A already requirs ~A"
		 (list namespace require-version)
		 #f)))

  (let ((available-versions (enumerate-versions namespace)))
    (unless available-versions
      (scm-error 'misc-error "require-versions"
		 "Namespace ~A not available"
		 (list namespace)
		 #f))
    (unless (member version available-versions)
      (scm-error 'misc-error "require-versions"
		 "Namespace ~A not available for versions ~A"
		 (list namespace version))))
  (set! _versions
    (assoc-set! _versions namespace version)))

(define* (require-foreign namespace #:optional symbol)
  "Ensure the given foreign marshaling module is available a loaded.
NAMESPACE - a string - Introspection namespace of the foreign module (e.g. \"cairo\")
SYMBOL - a string - Optional symbol typename to ensure a converter exists

Throws an error on failure.

Example:
  (import gi)
  (import cairo)
  (require-foreign \"cairo\")
"
  (error "unimplemented"))

(load-extension ".libs/libguile-gir.so" "gir_init")
