(use-modules (gi) (gi util)
             (srfi srfi-64)
             (rnrs bytevectors))

(use-typelibs (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip))))

(test-begin "byte-array")

(let ((barray #f))
  (test-assert "create bytearray"
    (set! barray (byte-array:new)))

  (test-assert "bytevector?"
    (bytevector? barray))

  (test-equal "zero-size"
    0
    (bytevector-length barray)))

(let ((bv (make-bytevector 4 32)))
  (test-equal "take bytevector"
    bv
    (byte-array:new-take bv)))

(let ((bytes #f))
  (test-assert "allocate 0 bytes"
    (begin (set! bytes (bytes:new #f))
           (is-a? bytes <GBytes>)))

  (test-eqv "size is 0"
    0
    (get-size bytes))

  (test-eq "data is #f"
    #f
    (get-data bytes)))

(test-end "byte-array")
