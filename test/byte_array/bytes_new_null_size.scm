(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (begin
   (let* ((self (bytes:new #f))
          (siz (with-object self (get-size))))
     (format #t "New Byte Array: ~S~%" self)
     (format #t "Size: ~S~%" siz)
     (and (gbytes? self)
          (equal? 0 siz)))))
