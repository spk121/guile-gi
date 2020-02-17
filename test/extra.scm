(use-modules (gi) (gi util)
             (rnrs bytevectors)
             (srfi srfi-64))

(use-typelibs ("Extra" "1.0")
              (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip format)))
              ("GObject" "2.0"))

(test-begin "extra")

(test-assert "zero-terminated-uint8-array-input bytevector"
  (zero-terminated-uint8-array-input? #vu8(1 2 3)))

(test-assert "zero-terminated-int8-array-input bytevector"
  (zero-terminated-int8-array-input? #vu8(1 2 3)))

(test-assert "zero-terminated-int16-array-input"
  (zero-terminated-int16-array-input? #s16(1 2 3)))

(test-assert "zero-terminated-int16-array-input-full"
  (zero-terminated-int16-array-input-full? #s16(1 2 3)))

(test-equal "zero-terminated-int8-array-output-full"
  #s8(9 8 7 6 5 4 3 2 1)
  (zero-terminated-int8-array-output-full))

(test-equal "zero-terminated-int16-array-output-full"
  #s16(9 8 7 6 5 4 3 2 1)
  (zero-terminated-int16-array-output-full))

(test-end "extra")
