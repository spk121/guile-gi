(use-modules (gi) (gi util) (gi types)
             (rnrs bytevectors)
             (srfi srfi-64)
             (ice-9 receive)
             (system foreign))

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

(test-skip   "callback-char")
(test-assert "callback-char"
  (call-callback-chars?
   (lambda (s8 u8 u32)
     (and (eqv? s8 #\a)
          (eqv? u8 #\b)
          (eqv? u32 #\c)))
   #\a #\b #\c))

(test-assert "callback-signed-ints"
  (call-callback-signed-ints?
   (lambda (s8 s16 s32 s64)
     (and (= s8 -1)
          (= s16 -2)
          (= s32 -3)
          (= s64 -4)))
   -1 -2 -3 -4))

(define (incr a b c d)
  (values (1+ a) (1+ b) (1+ c) (1+ d)))

(test-assert "callback-unsigned-ints"
  (call-callback-unsigned-ints?
   (lambda (u8 u16 u32 u64)
     (and (= u8 1)
          (= u16 2)
          (= u32 3)
          (= u64 4)))
   1 2 3 4))

(test-assert "callback-floats"
  (call-callback-floats?
   (lambda (f32 f64)
     (and (= f32 0.0)
          (= f64 1.0)))
   0.0 1.0))

(test-skip  "callback-unsigned-ints arity too low")
(test-error "callback-unsigned-ints arity too low"
            #t
  (call-callback-unsigned-ints?
   (lambda (u8 u16 u32) #t)
   1 2 3 4))

(test-skip  "callback-unsigned-ints arity too high")
(test-error "callback-unsigned-ints arity too high"
            #t
  (call-callback-unsigned-ints?
   (lambda (u8 u16 u32 u64 u128) #t)
   1 2 3 4))

(test-error "callback-unsigned-ints out of range"
            #t
  (call-callback-unsigned-ints?
   (lambda (u8 u16 u32 u64) #t)
   #xFFFF #xFFFF #xFFFF #xFFFF))

(test-error "callback-unsigned-ints wrong type"
            #t
  (call-callback-unsigned-ints?
   (lambda (u8 u16 u32 u64) #t)
   #t "hello" 1.0 #\x))

(test-equal "callback-out-signed-ints"
  (+ -1 -2 -3 -4)
  ;; The call-callback-out-signed-ints procedure returns the sum of
  ;; the output values passed by the callback.
  (call-callback-out-signed-ints
   (lambda ()
     (values -1 -2 -3 -4))))

(test-equal "callback-out-unsigned-ints"
  (+ 1 2 3 4)
  (call-callback-out-unsigned-ints
   (lambda ()
     (values 1 2 3 4))))

(test-skip 1)
(test-error "callback-out-unsigned-ints too few output args"
            #t
  (call-callback-out-unsigned-ints
   (lambda ()
     (values 1 2 3))))

(test-skip 1)
(test-error "callback-out-unsigned-ints too many output args"
            #t
  (call-callback-out-unsigned-ints
   (lambda ()
     (values 1 2 3 4 5))))

(test-error "callback-out-unsigned-ints out of range"
            #t
  (call-callback-out-unsigned-ints
   (lambda ()
     (values #xFFFF #xFFFF #xFFFF #xFFFF))))

(test-error "callback-out-unsigned-ints wrong type"
            #t
  (call-callback-out-unsigned-ints
   (lambda ()
     (values #t "hello" 0.0 #\x))))

(test-skip  "call-callback-char-passthrough")
(test-equal "call-callback-char-passthrough"
  #\x
  (call-callback-char-passthrough
   (lambda (c)
     c)
   #\x))

(define (cb-return-uints)
  (values 1 2 3 4))

(test-assert "test that callbacks are registered"
  (begin
    (call-callback-out-unsigned-ints cb-return-uints)
    (is-registered-callback? cb-return-uints)))

(test-assert "test that callbacks have closure pointers"
  (begin
    (call-callback-out-unsigned-ints cb-return-uints)
    (pointer? (get-registered-callback-closure-pointer cb-return-uints))))

(test-end "extra")
