(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0") ("Gtk" "3.0"))

;; This test exercises float32 argument conversions and return types.
(automake-test
 (begin
   (init)
   (= 0
      (with-object (entry:new)
        (set-alignment 0)
        (get-alignment)))))
