(use-modules (gi)
             (gi glib-2)
             (test automake-test-lib))

;; This test exercises float32 argument conversions and return types.

(automake-test
 (if (not (false-if-exception (typelib-load "Gtk" "3.0")))
     'skipped
     ;; else
     (begin
       (init 0 #f)
       (let ([entry1 (cast (entry:new) <GtkEntry>)]
             [entry2 (cast (entry:new) <GtkEntry>)])
         (with-object entry1 (set-alignment 1.0))
         (with-object entry2 (set-alignment 0.5))
         (format #t "entry 1: ~S~%" entry1)
         (format #t "entry 1 alignment: ~S~%" (with-object entry1 (get-alignment)))
         (format #t "entry 2: ~S~%" entry2)
         (format #t "entry 2 alignment: ~S~%" (with-object entry2 (get-alignment)))
         (and (= 1.0 (with-object entry1 (get-alignment)))
              (= 0.5 (with-object entry2 (get-alignment))))))))
