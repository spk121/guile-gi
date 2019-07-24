(use-modules (gi)
             (ice-9 receive)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0"))

(automake-test
 (begin
   (let ((self (bytes:new #f)))
     (let ((data (with-object self (get-data))))
       (format #t "New Byte Array: ~S~%" self)
       (format #t "Data: ~S~%" data)
       (not data)))))

