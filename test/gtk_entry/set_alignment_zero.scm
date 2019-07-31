(use-modules (gi) (gi util)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0")
                 (("Gtk" "3.0") #:renamer (protect* %rnrs-syntax)))

;; This test exercises float32 argument conversions and return types.
(automake-test
 (begin
   (init)
   (let ([entry (entry:new)])
     (set-alignment entry 0)
     (format #t "entry: ~S~%" entry)
     (format #t "entry alignment: ~S~%" (get-alignment entry))
     (= 0 (get-alignment entry)))))
