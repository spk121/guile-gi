(use-modules (gi) (gi util)
             (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0") ("GObject" "2.0")
                 (("Gtk" "3.0") #:renamer (protect* %rnrs-syntax)))

(automake-test
 (begin
   (init)
   (let* ([box1 (box:new ORIENTATION_VERTICAL 1)]
          [box2 (make <GtkBox>
                  #:orientation ORIENTATION_VERTICAL
                  #:spacing 2)])
     (format #t "box 1: ~S~%" box1)
     (format #t "box 1 spacing: ~S~%" (spacing box1))
     (format #t "box 2: ~S~%" box2)
     (format #t "box 2 spacing: ~S~%" (spacing box2))
     (and (is-a? box1 <GtkBox>)
          (= 1 (spacing box1))
          (is-a? box2 <GtkBox>)
          (= 2 (spacing box2))))))
