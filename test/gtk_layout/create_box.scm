(use-modules (gi)
             (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0") ("Gtk" "3.0"))

(automake-test
 (begin
   (init 0 #f)
   (let* ([box1 (cast (box:new ORIENTATION_VERTICAL 1) <GtkBox>)]
          [box2 (create <GtkBox>
                  (orientation ORIENTATION_VERTICAL)
                  (spacing 2))])
     (format #t "box 1: ~S~%" box1)
     (format #t "box 1 spacing: ~S~%" (with-object box1 (get-spacing)))
     (format #t "box 2: ~S~%" box2)
     (format #t "box 2 spacing: ~S~%" (with-object box2 spacing))
     (and (gtk-box? box1)
          (= 1 (with-object box1 (get-spacing)))
          (gtk-box? box2)
          (= 2 (with-object box2 spacing))))))
