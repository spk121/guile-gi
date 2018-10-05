(define-module (gir _ir)
  #:export (%irepository-enumerate-versions
	    %irepository-get-default
	    %irepository-require))

;; This is just a placeholder module for everytying exported by the C
;; library.  Everyting in the C library is supposed to export itself.

(load-extension "libguile-gi.so" "gir_module_init")
