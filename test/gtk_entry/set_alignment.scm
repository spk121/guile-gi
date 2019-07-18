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
     (with-object entry1 (set-alignment 1.0))
     (with-object entry2 (set-alignment 0.5))
     (format #t "entry 1: ~S~%" entry1)
     (format #t "entry 1 alignment: ~S~%" (with-object entry1 (get-alignment)))
     (format #t "entry 2: ~S~%" entry2)
     (format #t "entry 2 alignment: ~S~%" (with-object entry2 (get-alignment)))
     (and (= 1.0 (with-object entry1 (get-alignment)))
          (= 0.5 (with-object entry2 (get-alignment)))))))
