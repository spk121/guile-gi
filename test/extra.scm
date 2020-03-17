(use-modules (gi) (gi util) (gi types)
             (rnrs bytevectors)
             (oop goops)
             (srfi srfi-64)
             (ice-9 receive)
             (system foreign))

(use-typelibs ("Extra" "1.0")
              (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip format)))
              ("GObject" "2.0"))

(test-begin "extra")

(define n-functions 0)
(define n-callbacks 0)
(define n-c-callbacks 0)

(test-assert "bind %before-function-hook"
  (begin
    (add-hook! %before-function-hook
               (lambda (fn args)
                 (set! n-functions (1+ n-functions))))))

(test-assert "bind %before-callback-hook"
  (begin
    (add-hook! %before-callback-hook
               (lambda (cb proc args)
                 (set! n-callbacks (1+ n-callbacks))))))

(test-assert "bind %before-c-callback-hook"
  (begin
    (add-hook! %before-c-callback-hook
               (lambda (cb proc args)
                 (set! n-c-callbacks (1+ n-c-callbacks))))))

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

(test-expect-fail "callback-char")
(test-assert "callback-char"
  (call-callback-chars?
   (lambda (s8 u8 u32)
     (and (eqv? s8 #\a)
          (eqv? u8 #\b)
          (eqv? u32 #\c)))
   #\a #\b #\c))

(test-assert "callback-signed-ints"
  (call-callback-signed-ints?
   (lambda (s ss ls s8 s16 s32 s64)
     (and (= s -1)
          (= ss -2)
          (= ls -3)
          (= s8 -4)
          (= s16 -5)
          (= s32 -6)
          (= s64 -7)))
   -1 -2 -3 -4 -5 -6 -7))

(test-error "callback-signed-ints inputs out of range"
            #t
            (call-callback-signed-ints?
             (lambda (s ss ls s8 s16 s32 s64)
               (and (= s -1)
                    (= ss -2)
                    (= ls -3)
                    (= s8 -4)
                    (= s16 -5)
                    (= s32 -6)
                    (= s64 -7)))
             (expt 2 65) -2 -3 -4 -5 -6 -7))

(test-error "callback-signed-ints inputs wrong type"
            #t
            (call-callback-signed-ints?
             (lambda (s ss ls s8 s16 s32 s64)
               (and (= s -1)
                    (= ss -2)
                    (= ls -3)
                    (= s8 -4)
                    (= s16 -5)
                    (= s32 -6)
                    (= s64 -7)))
             'a 'b 'c 'd 'e 'f 'g))

(test-error "callback-signed-ints callback arity too low"
            #t
            (call-callback-signed-ints?
             (lambda (x)
               #t)
             -1 -2 -3 -4 -5 -6 -7))

(test-error "callback-signed-ints callback arity too high"
            #t
            (call-callback-signed-ints?
             (lambda (s ss ls s8 s16 s32 s64 s128)
               #t)
             -1 -2 -3 -4 -5 -6 -7))

(test-error "callback-signed-ints too few arguments"
            #t
            (call-callback-signed-ints?
             (lambda (s ss ls s8 s16 s32 s64)
               #t)
             -1))

(test-error "callback-signed-ints too many arguments"
            #t
            (call-callback-signed-ints?
             (lambda (s ss ls s8 s16 s32 s64)
               #t)
             -1 -2 -3 -4 -5 -6 -7 -8))

(test-error "callback-signed-ints callback return val missing"
            #t
            (call-callback-signed-ints?
             (lambda (s ss ls s8 s16 s32 s64)
               (when #f #t))
             -1 -2 -3 -4 -5 -6 -7))

(test-assert "callback-unsigned-ints"
  (call-callback-unsigned-ints?
   (lambda (u su lu u8 u16 u32 u64)
     (and (= u 1)
          (= su 2)
          (= lu 3)
          (= u8 4)
          (= u16 5)
          (= u32 6)
          (= u64 7)))
   1 2 3 4 5 6 7))

(test-error "callback-unsigned-ints arity too low"
            #t
            (call-callback-unsigned-ints?
             (lambda (u su lu u8 u16 u32) #t)
             1 2 3 4 5 6 7))

(test-error "callback-unsigned-ints arity too high"
            #t
            (call-callback-unsigned-ints?
             (lambda (u su lu u8 u16 u32 u64 u128) #t)
             1 2 3 4 5 6 7))

(test-error "callback-unsigned-ints out of range"
            #t
            (call-callback-unsigned-ints?
             (lambda (u su lu u8 u16 u32 u64) #t)
             #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF))

(test-error "callback-unsigned-ints wrong type"
            #t
            (call-callback-unsigned-ints?
             (lambda (u su lu u8 u16 u32 u64) #t)
             1 2 3 #t "hello" 1.0 #\x))

(test-assert "callback-floats"
  (call-callback-floats?
   (lambda (f32 f64)
     (and (= f32 0.0)
          (= f64 1.0)))
   0.0 1.0))

(define (cb-strings utf8string localestring)
  (and (string=? utf8string "foo")
       (string=? localestring "bar")))

(test-assert "callback-const-strings-const"
  (call-callback-const-strings-const?
   cb-strings
   "foo" "bar"))

(test-assert "callback-const-strings"
  (call-callback-const-strings?
   cb-strings
   "foo" "bar"))

(test-assert "callback-strings"
  (call-callback-strings?
   cb-strings
   "foo" "bar"))

(test-assert "callback-pointers"
  (let* ((obj (make <GObject>))
         (ptr (bytevector->pointer (make-bytevector 8))))
    (define-method (cb-pointers (_obj <GObject>) _ptr)
      (and (equal? _ptr ptr)
           (equal? (slot-ref _obj 'value) (slot-ref obj 'value))))
    (call-callback-pointers? cb-pointers obj ptr)))

(test-equal "callback-out-signed-ints"
  (+ -1 -2 -3 -4 -5 -6 -7)
  ;; The call-callback-out-signed-ints procedure returns the sum of
  ;; the output values passed by the callback.
  (call-callback-out-signed-ints
   (lambda ()
     (values -1 -2 -3 -4 -5 -6 -7))))

(test-equal "callback-out-unsigned-ints"
  (+ 1 2 3 4)
  (call-callback-out-unsigned-ints
   (lambda ()
     (values 1 2 3 4))))

(test-error "callback-out-unsigned-ints too few output args"
            #t
            (call-callback-out-unsigned-ints
             (lambda ()
               (values 1 2 3))))

(test-expect-fail "callback-out-unsigned-ints too many output args")
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

(test-equal "callback-out-floats"
  (+ 1.0 2.0)
  (call-callback-out-floats
   (lambda ()
     (values 1.0 2.0))))

(test-assert "callback-out-strings"
  (call-callback-out-strings?
   (lambda()
     (values "foo" "bar"))))

(test-expect-fail "call-callback-char-passthrough")
(test-equal "call-callback-char-passthrough"
  #\x
  (call-callback-char-passthrough
   (lambda (c)
     c)
   #\x))

(test-equal "call-callback-pointer-passthrough"
  "hello"
  (pointer->string
   (call-callback-pointer-passthrough
    (lambda (c)
      c)
    (string->pointer "hello"))))

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

(test-assert "return a c callback"
  (let ((cb (return-callback)))
    (procedure? cb)))

;; This calls an introspected C function that returns a C callback,
;; and then executes that callback.  The callback is just an integer
;; passthrough.
(test-equal "call returned c callback"
  1
  ((return-callback) 1))

(test-error "call returned c callback with too few arguments"
            #t
            ((return-callback)))

(test-error "call returned c callback with too many arguments"
            #t
            ((return-callback) 1 2))

(test-error "call returned c callback with wrong type"
            #t
            ((return-callback) "hello"))

(test-equal "hooks ran"
  '(38 25 4)
  (list n-functions
        n-callbacks
        n-c-callbacks))

(test-end "extra")
