(use-modules (gi)
             (rnrs bytevectors)
             (test automake-test-lib))

(automake-test
 (begin
   (import-typelib "GLib" "2.0")
   (let* ((B (bytes:new #vu8(1 2 3 4))))
     (format #t "New Bytes: ~S~%" B)
     #f)))
