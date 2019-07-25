(use-modules (gi) (gi util)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0")
                 (("Gtk" "3.0") #:renamer (protect* %rnrs-syntax)))

;; This test exercises float32 argument conversions and return types.

(automake-test
 (begin
   (init)
   (let ([entry1 (entry:new)]
         [entry2 (entry:new)])
     (set-alignment entry1 1.0)
     (set-alignment entry2 0.5)
     (format #t "entry 1: ~S~%" entry1)
     (format #t "entry 1 alignment: ~S~%" (get-alignment entry1))
     (format #t "entry 2: ~S~%" entry2)
     (format #t "entry 2 alignment: ~S~%" (get-alignment entry2))
     (and (= 1.0 (get-alignment entry1))
          (= 0.5 (get-alignment entry2))))))
