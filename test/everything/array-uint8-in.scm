(use-modules (gi)
             (rnrs bytevectors)
             (test automake-test-lib))

(typelib-require ("Marshall" "1.0"))

(automake-test
 (let ((in (u8-list->bytevector (list (char->integer #\a)
                                      (char->integer #\b)
                                      (char->integer #\c)
                                      (char->integer #\d)))))
   (write in)
   (newline)
   (array-uint8-in in)))
