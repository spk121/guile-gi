(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib))

(automake-test
 (let ((cksum (checksum:new CHECKSUM_SHA256))
       (bv (string->utf8 "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n")))
   (write cksum) (newline)
   (write bv) (newline)
   (with-object cksum (update bv))
   (let ((output (with-object cksum (get-string))))
     (write output) (newline)
     (string=?
      output
      "a06b168d8e72c069aa3cc58d64b92a300f9f82127facb3219855053e49a4ecbe"))))
