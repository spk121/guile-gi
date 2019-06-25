(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (system foreign)
             (test automake-test-lib))

(automake-test
 (let* ([key (string->utf8 "key")]
        [key-len (bytevector-length key)]
        [bv (string->utf8 "The quick brown fox jumps over the lazy dog")]
        [bv-len (bytevector-length bv)]
        [bytes-key (Bytes-new-take key key-len)]
        [bytes (Bytes-new-take bv bv-len)]
        [hmac (compute-hmac-for-bytes
               CHECKSUM_TYPE_MD5
               bytes-key
               bytes)])
   (write key) (newline)
   (write key-len) (newline)
   (write bv) (newline)
   (write bv-len) (newline)
   (write bytes-key) (newline)
   (write bytes) (newline)
   (write hmac) (newline)
   (string=? hmac "80070713463e7749b90c2dc24911e275")))
