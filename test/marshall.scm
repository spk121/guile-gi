(use-modules (ice-9 receive)
             (ice-9 hash-table)

             (oop goops)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-43)
             (srfi srfi-64)

             (system foreign)

             (gi)
             (gi types)
             (gi util))

(test-begin "marshall.scm")


;; We know, that this Marshall exists, because we don't run the test
;; if it does not. GLib is an explicit dependency, so it too always exists
(use-typelibs ;; (("GObject" "2.0") #:select ())
              (("Marshall" "1.0")
               #:renamer (protect* '(sizeof short int long size_t)))
              (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip int))))

(define-syntax-rule (utf8-inout f)
  (test-assert (symbol->string (quote f))
    (string-null? (f CONSTANT_UTF8))))


(define-syntax-rule (utf8-output f)
  (test-assert (symbol->string (quote f))
    (string=? (f) CONSTANT_UTF8)))

(utf8-inout utf8-none-inout!)
(utf8-inout utf8-full-inout!)
(utf8-output utf8-none-out)
(utf8-output utf8-full-out)
(utf8-output utf8-none-return)
(utf8-output utf8-full-return)

(test-assert "boolean-in-false"
  (begin
    (boolean-in-false #f)
    #t))

(test-assert "boolean-return-false"
  (not (boolean-return-false?)))

(test-assert "boolean-out-false"
  (not (boolean-out-false)))

(test-assert "boolean-in-true"
  (begin
    (boolean-in-true #t)
    #t))

(test-assert "boolean-return-true"
  (boolean-return-true?))

(test-assert "boolean-out-true"
  (boolean-out-true))

(test-assert "boolean-inout-false-true"
  (boolean-inout-false-true #f))

(test-assert "boolean-inout-true-false"
  (not (boolean-inout-true-false #t)))


(test-assert "int-max-in"
  (every
   (compose (const #t) (cute <> <>))
   (list int8-in-max
         int16-in-max
         int32-in-max
         int64-in-max
         short-in-max
         int-in-max
         long-in-max)

   `(,(1- (expt 2 7))
     ,(1- (expt 2 15))
     ,(1- (expt 2 31))
     ,(1- (expt 2 63))
     ,(1- (expt 2 (1- (* 8 (sizeof short)))))
     ,(1- (expt 2 (1- (* 8 (sizeof int)))))
     ,(1- (expt 2 (1- (* 8 (sizeof long))))))))

(test-assert "int-max-out"
  (every
   =
   (map
    (cute <>)
    (list int8-return-max
          int16-return-max
          int32-return-max
          int64-return-max
          short-return-max
          int-return-max
          long-return-max
          ssize-return-max))

   (map
    (cute <>)
    (list int8-out-max
          int16-out-max
          int32-out-max
          int64-out-max
          short-out-max
          int-out-max
          long-out-max
          ssize-out-max))

   `(,(1- (expt 2 7))
     ,(1- (expt 2 15))
     ,(1- (expt 2 31))
     ,(1- (expt 2 63))
     ,(1- (expt 2 (1- (* 8 (sizeof short)))))
     ,(1- (expt 2 (1- (* 8 (sizeof int)))))
     ,(1- (expt 2 (1- (* 8 (sizeof long)))))
     ,(1- (expt 2 (1- (* 8 (sizeof size_t))))))))

(test-assert "int-min-in"
  (every
   (compose (const #t) (cute <> <>))
   (list int8-in-min
         int16-in-min
         int32-in-min
         int64-in-min
         short-in-min
         int-in-min
         long-in-min)

   `(,(- (expt 2 7))
     ,(- (expt 2 15))
     ,(- (expt 2 31))
     ,(- (expt 2 63))
     ,(- (expt 2 (1- (* 8 (sizeof short)))))
     ,(- (expt 2 (1- (* 8 (sizeof int)))))
     ,(- (expt 2 (1- (* 8 (sizeof long))))))))

(test-assert "int-min-out"
  (every
   =
   (map
    (cute <>)
    (list int8-return-min
          int16-return-min
          int32-return-min
          int64-return-min
          short-return-min
          int-return-min
          long-return-min
          ssize-return-min))

   (map
    (cute <>)
    (list int8-out-min
          int16-out-min
          int32-out-min
          int64-out-min
          short-out-min
          int-out-min
          long-out-min
          ssize-out-min))

   `(,(- (expt 2 7))
     ,(- (expt 2 15))
     ,(- (expt 2 31))
     ,(- (expt 2 63))
     ,(- (expt 2 (1- (* 8 (sizeof short)))))
     ,(- (expt 2 (1- (* 8 (sizeof int)))))
     ,(- (expt 2 (1- (* 8 (sizeof long)))))
     ,(- (expt 2 (1- (* 8 (sizeof size_t))))))))

(test-assert "int-max-min"
  (every
   (lambda (f v e)
     (= (f v) e))
   (list int8-inout-max-min
         int16-inout-max-min
         int32-inout-max-min
         int64-inout-max-min
         short-inout-max-min
         int-inout-max-min
         long-inout-max-min)

   `(,(1- (expt 2 7))
     ,(1- (expt 2 15))
     ,(1- (expt 2 31))
     ,(1- (expt 2 63))
     ,(1- (expt 2 (1- (* 8 (sizeof short)))))
     ,(1- (expt 2 (1- (* 8 (sizeof int)))))
     ,(1- (expt 2 (1- (* 8 (sizeof long))))))


   `(,(- (expt 2 7))
     ,(- (expt 2 15))
     ,(- (expt 2 31))
     ,(- (expt 2 63))
     ,(- (expt 2 (1- (* 8 (sizeof short)))))
     ,(- (expt 2 (1- (* 8 (sizeof int)))))
     ,(- (expt 2 (1- (* 8 (sizeof long))))))))


(test-assert "int-min-max"
  (every
   (lambda (f v e)
     (= (f v) e))
   (list int8-inout-min-max
         int16-inout-min-max
         int32-inout-min-max
         int64-inout-min-max
         short-inout-min-max
         int-inout-min-max
         long-inout-min-max)

   `(,(- (expt 2 7))
     ,(- (expt 2 15))
     ,(- (expt 2 31))
     ,(- (expt 2 63))
     ,(- (expt 2 (1- (* 8 (sizeof short)))))
     ,(- (expt 2 (1- (* 8 (sizeof int)))))
     ,(- (expt 2 (1- (* 8 (sizeof long))))))

   `(,(1- (expt 2 7))
     ,(1- (expt 2 15))
     ,(1- (expt 2 31))
     ,(1- (expt 2 63))
     ,(1- (expt 2 (1- (* 8 (sizeof short)))))
     ,(1- (expt 2 (1- (* 8 (sizeof int)))))
     ,(1- (expt 2 (1- (* 8 (sizeof long))))))))

(test-assert "uint-in"
  (every
   (compose (const #t) (cute <> <>))
   (list uint8-in
         uint16-in
         uint32-in
         uint64-in
         ushort-in
         uint-in
         ulong-in)

   `(,(1- (expt 2 8))
     ,(1- (expt 2 16))
     ,(1- (expt 2 32))
     ,(1- (expt 2 64))
     ,(1- (expt 2 (* 8 (sizeof short))))
     ,(1- (expt 2 (* 8 (sizeof int))))
     ,(1- (expt 2 (* 8 (sizeof long)))))))

(test-assert "uint-inout"
  (every
   (lambda (f v)
     (= (f v) 0))
   (list uint8-inout
         uint16-inout
         uint32-inout
         uint64-inout
         ushort-inout
         uint-inout
         ulong-inout)

   `(,(1- (expt 2 8))
     ,(1- (expt 2 16))
     ,(1- (expt 2 32))
     ,(1- (expt 2 64))
     ,(1- (expt 2 (* 8 (sizeof short))))
     ,(1- (expt 2 (* 8 (sizeof int))))
     ,(1- (expt 2 (* 8 (sizeof long)))))))

(test-assert "uint-out"
  (every =
         (map
          (cute <>)
          (list uint8-return
                uint16-return
                uint32-return
                uint64-return
                ushort-return
                uint-return
                ulong-return
                size-return))
         (map
          (cute <>)
          (list uint8-out
                uint16-out
                uint32-out
                uint64-out
                ushort-out
                uint-out
                ulong-out
                size-out))

         `(,(1- (expt 2 8))
           ,(1- (expt 2 16))
           ,(1- (expt 2 32))
           ,(1- (expt 2 64))
           ,(1- (expt 2 (* 8 (sizeof short))))
           ,(1- (expt 2 (* 8 (sizeof int))))
           ,(1- (expt 2 (* 8 (sizeof long))))
           ,(1- (expt 2 (* 8 (sizeof size_t)))))))

(test-assert "time-t-out"
  (= (time-t-out) 1234567890))

(test-assert "utf8-dangling-out"
  (string? (utf8-dangling-out)))

(test-assert "callback-return-value-only"
  (equal? 123
          ;; This procedure 'callback-return-value-only' returns the
          ;; value returned by the callback.
          (callback-return-value-only
           (lambda ()
             123))))

(test-equal "callback-one-out-parameter"
  1234.5
  (callback-one-out-parameter
   (lambda ()
     1234.5)))

(test-equal "callback-one-out-parameter"
  1234.5
  (callback-one-out-parameter
   (lambda ()
     1234.5)))

(test-equal "callback-mutiple-out-parameters"
  '(1.5 2.5)
  ((compose
    list
    callback-multiple-out-parameters)
   (lambda () (values 1.5 2.5))))

(test-equal "callback-return-value-and-one-out-parameter"
  '(1 2)
  ((compose
    list
    callback-return-value-and-one-out-parameter)
   (lambda () (values 1 2))))

(test-equal "callback-return-value-and-multiple-out-parameters"
  '(1 2 3)
  ((compose
    list
    callback-return-value-and-multiple-out-parameters)
   (lambda () (values 1 2 3))))

(define (hash-contains? hash key-list val-list)
  (and (= (length key-list) (hash-count (const #t) hash))
       (every
        (lambda (key val)
          (equal? (hash-ref hash key) val))
        key-list
        val-list)))

(define (hash-contains-strings? hash)
  (hash-contains? hash '("-1" "0" "1" "2") '("1" "0" "-1" "-2")))

(test-assert "ghashtable-int-none-return"
  (hash-contains? (ghashtable-int-none-return) '(-1 0 1 2) '(1 0 -1 -2)))

(test-assert "ghashtable-utf8-none-return"
  (hash-contains-strings? (ghashtable-utf8-none-return)))

(test-assert "ghashtable-utf8-container-return"
  (hash-contains-strings? (ghashtable-utf8-container-return)))

(test-assert "ghashtable-utf8-full-return-return"
  (hash-contains-strings? (ghashtable-utf8-container-return)))

(test-assert "ghashtable-int-none-in"
  (let ((H (alist->hash-table
            '((-1 . 1)
              (0 . 0)
              (1 . -1)
              (2 . -2)))))
    (ghashtable-int-none-in H)
    #t))

(test-assert "ghashtable-utf8-none-in"
  (let ((H (alist->hash-table
            '(("-1" . "1")
              ("0" . "0")
              ("1" . "-1")
              ("2" . "-2")))))
    (ghashtable-utf8-none-in H)
    #t))

(test-assert "ghashtable-double-in"
  (let ((H (alist->hash-table
            '(("-1" . -0.1)
              ("0" . 0.0)
              ("1" . 0.1)
              ("2" . 0.2)))))
    (ghashtable-double-in H)
    #t))

(test-assert "ghashtable-float-in"
  (let ((H (alist->hash-table
            '(("-1" . -0.1)
              ("0" . 0.0)
              ("1" . 0.1)
              ("2" . 0.2)))))
    (ghashtable-float-in H)
    #t))

(test-assert "ghashtable-int64-in"
  (let ((H (alist->hash-table
            '(("-1" . -1)
              ("0" . 0)
              ("1" . 1)
              ("2" . #x100000000)))))
    (ghashtable-int64-in H)
    #t))

(test-assert "ghashtable-uint64-in"
  (let ((H (alist->hash-table
            '(("-1" . #x100000000)
              ("0" . 0)
              ("1" . 1)
              ("2" . 2)))))
    (ghashtable-uint64-in H)
    #t))

(test-assert "ghashtable-utf8-none-out"
  (hash-contains-strings? (ghashtable-utf8-none-out)))

(test-assert "ghashtable-utf8-container-out"
  (hash-contains-strings? (ghashtable-utf8-container-out)))

(test-assert "ghashtable-utf8-full-out"
  (hash-contains-strings? (ghashtable-utf8-full-out)))

(define (hash-utf8-inout? proc)
  (let* ((H-in (alist->hash-table
                '(("-1" . "1")
                  ("0" . "0")
                  ("1" . "-1")
                  ("2" . "-2"))))
         (H-out (proc H-in)))
    (hash-contains? H-out
                    '("-1" "0" "1") '("1" "0" "1"))))

(test-assert "ghashtable-utf8-full-inout"
  (hash-utf8-inout? ghashtable-utf8-full-inout!))

(test-assert "ghashtable-utf8-container-inout"
  (hash-utf8-inout? ghashtable-utf8-container-inout!))

(test-assert "ghashtable-utf8-none-inout"
  (hash-utf8-inout? ghashtable-utf8-none-inout!))

(define (make-value type value)
  (let ((v (make <GValue>)))
    (set! (v type) value)
    v))

(test-equal "gvalue-return-class"
  <GValue>
  (class-of (gvalue-return)))

;; TODO: Inside `guix environment`, this test appears to fail.
(unless (test-passed?)
  ;; skip the value and closure tests that follow -- they are broken
  (test-skip 12))

(test-equal "gvalue-return"
  42
  ((gvalue-return)))

(test-equal "gvalue-out"
  42
  ((gvalue-out)))

;; TODO: Expected a <GValue>, but got *unspecified*
(test-expect-fail "gvalue-out-caller-allocates")
(test-equal "gvalue-out-caller-allocates"
  42
  ((gvalue-out-caller-allocates (make <GValue>))))

(test-equal "gvalue-inout"
  "42"
  ((gvalue-inout! (make-value G_TYPE_INT 42))))

(test-assert "gvalue-in"
  (begin
    (gvalue-in (make-value G_TYPE_INT 42))
    #t))

(test-assert "gvalue-in-with-type"
  (begin
    (gvalue-in-with-type (make-value G_TYPE_INT 42) G_TYPE_INT)
    #t))

(test-equal "gvalue-in-with-modification"
  24
  (let ((v (make-value G_TYPE_INT 42)))
    (gvalue-in-with-modification v)
    (v)))

(test-assert "gvalue-int64-in"
  (begin
    (gvalue-int64-in (make-value G_TYPE_INT64 (1- (expt 2 63))))
    #t))

(test-equal "gvalue-int64-out"
  (1- (expt 2 63))
  ((gvalue-int64-out)))

(test-assert "gclosure-in"
  (begin
    (gclosure-in (procedure->closure (const 42)))
    #t))

(test-equal "gclosure-return"
  42
  (((gclosure-return) G_TYPE_INT)))

(test-assert "gclosure-return->in"
  (begin
    (gclosure-in (gclosure-return))
    #t))

(test-end "marshall.scm")
