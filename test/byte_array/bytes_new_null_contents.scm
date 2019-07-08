(use-modules (gi)
             (gi glib-2)
             (ice-9 receive)
             (test automake-test-lib))

(automake-test
 (begin
   (let ((self (bytes:new #f)))
     (receive (data siz)
         (with-object self (get-data))
       (format #t "New Byte Array: ~S~%" self)
       (format #t "Data: ~S~%" data)
       (format #t "Size: ~S~%" siz)
       (and (not data)
            (equal? 0 siz))))))
