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
       (let ([entry (cast (entry:new) <GtkEntry>)])
         (send entry (set-alignment 0))
         (format #t "entry: ~S~%" entry)
         (format #t "entry alignment: ~S~%" (send entry (get-alignment)))
         (= 0 (send entry (get-alignment)))))))
