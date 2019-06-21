(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib))

(automake-test
 (let ((cksum (Checksum-new CHECKSUM_TYPE_SHA256))
       (bv (string->utf8 "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n")))
   (write cksum) (newline)
   (write bv) (newline)
   (send cksum (update bv -1))
   (let ((output (send cksum (get-string))))
     (write output) (newline)
     (string=?
      output
      "a06b168d8e72c069aa3cc58d64b92a300f9f82127facb3219855053e49a4ecbe"))))
