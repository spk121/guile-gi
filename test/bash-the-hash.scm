(use-modules (gi) (gi util)
             (srfi srfi-64)
             (rnrs bytevectors)
             (system foreign))

(use-typelibs (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip))))

(test-begin "checksum")

(test-equal "checksum"
  "a06b168d8e72c069aa3cc58d64b92a300f9f82127facb3219855053e49a4ecbe"
  (let ((cksum (checksum:new (symbol->checksum-type 'sha256)))
        (bv (string->utf8 "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n")))
    (update cksum bv)
    (get-string cksum)))

(test-end "checksum")

(test-begin "base64")

(test-equal "encode Base64"
  "AQIDBAU="
  (base64-encode #vu8(1 2 3 4 5)))

(test-equal "decode Base64"
  #u8(1 2 3 4 5)
  (base64-decode "AQIDBAU="))

(test-expect-fail "decode Base64 inplace")
(test-equal "decode Base64 inplace"
  #u8(1 2 3 4 5)
  (base64-decode-inplace! (string->utf8 "AQIDBAU=")))

(test-end "base64")

(test-begin "hmac")

(define hmac-key "key")
(define hmac-str "The quick brown fox jumps over the lazy dog")

(test-equal "HMAC(string)"
  "80070713463e7749b90c2dc24911e275"
  (compute-hmac-for-string
   (symbol->checksum-type 'md5)
   (string->utf8 hmac-key)
   hmac-str -1))

(test-equal "HMAC(bytevector)"
  "80070713463e7749b90c2dc24911e275"
  (compute-hmac-for-data
   (symbol->checksum-type 'md5)
   (string->utf8 hmac-key)
   (string->utf8 hmac-str)))

(test-equal "HMAC(GBytes)"
  "80070713463e7749b90c2dc24911e275"
  (compute-hmac-for-bytes
   (symbol->checksum-type 'md5)
   (bytes:new-take (string->utf8 hmac-key))
   (bytes:new-take (string->utf8 hmac-str))))

(test-end "hmac")
