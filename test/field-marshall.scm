;;; Field accessors with the types GIMarshallingTests has.

(use-modules (gi) (gi types) (gi util)
             (oop goops)
             (srfi srfi-64))

(test-begin "field-marshall")

(use-typelibs (("Marshall" "1.0") #:renamer (protect* '(sizeof short int long size_t)))
              (("GLib" "2.0") #:renamer (protect* '(test-equal test-assert test-skip int))))

(test-equal "number, string and string array fields"
  '(42 "hello" #("0" "1" "2"))
  (let ((b (boxed-struct:returnv)))
    (list (boxed-struct:long b) (boxed-struct:string b) (boxed-struct:g-strv b))))

(test-equal "number setter"
  9
  (let ((b (boxed-struct:returnv)))
    (set! (boxed-struct:long b) 9)
    (boxed-struct:long b)))

(test-assert "pointer fields are read-only"
  ;; The typelib doesn't say who would own a new string.
  (not (or (defined? 'boxed-struct:set-string!)
           (defined? 'boxed-struct:set-g-strv!))))

(test-equal "union fields"
  77
  (let ((u (make <MarshallUnion>)))
    (union:set-long! u 77)
    (union:long u)))

(test-assert "strings are copies"
  (let ((s (boxed-struct:string (boxed-struct:returnv))))
    (gc) (gc)
    (equal? s "hello")))

(test-end "field-marshall")
