(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib))

(automake-test
 (let ((bv (u8-list->bytevector '(1 2 3 4 5))))
   (let ((str (base64-encode bv)))
     (write bv) (newline)
     (write str) (newline)
     (string? str))))
