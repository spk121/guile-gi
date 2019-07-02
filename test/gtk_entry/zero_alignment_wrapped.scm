(use-modules (gi)
             (gi glib-2)
             (test automake-test-lib))

;; This test exercises float32 argument conversions and return types.
(automake-test
 (if (not (false-if-exception (typelib-load "Gtk" "3.0")))
     'skipped
     (begin
       (init 0 #f)
       (= 0
          (with-object (cast (entry:new) <GtkEntry>)
            (set-alignment 0)
            (get-alignment))))))
