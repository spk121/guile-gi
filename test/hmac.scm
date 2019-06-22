(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (test automake-test-lib))

(automake-test
 (let* ([str "The quick brown fox jumps over the lazy dog"]
        [hmac (compute-hmac-for-string
               CHECKSUM_TYPE_MD5
               (string->utf8 "key") 3
               str -1)])
   (write str) (newline)
   (write hmac) (newline)
   (string=? hmac "80070713463e7749b90c2dc24911e275")))
