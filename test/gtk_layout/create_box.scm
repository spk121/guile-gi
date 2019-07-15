(use-modules (gi)
             (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0") ("GObject" "2.0") ("Gtk" "3.0"))

(automake-test
 (begin
   (init)
   (let* ([box1 (box:new ORIENTATION_VERTICAL 1)]
          [box2 (make <GtkBox>
                  #:orientation ORIENTATION_VERTICAL
                  #:spacing 2)])
     (format #t "box 1: ~S~%" box1)
     (format #t "box 1 spacing: ~S~%" (with-object box1 (get-spacing)))
     (format #t "box 2: ~S~%" box2)
     (format #t "box 2 spacing: ~S~%" (with-object box2 spacing))
     (and (is-a? box1 <GtkBox>)
          (= 1 (with-object box1 (get-spacing)))
          (is-a? box2 <GtkBox>)
          (= 2 (with-object box2 spacing))))))
