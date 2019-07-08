(use-modules (gi)
             (gi glib-2)
             (test automake-test-lib))

(automake-test
 (begin
   (let* ((self (bytes:new #f))
          (siz (with-object self (get-size))))
     (format #t "New Byte Array: ~S~%" self)
     (format #t "Size: ~S~%" siz)
     (and (gbytes? self)
          (equal? 0 siz)))))
