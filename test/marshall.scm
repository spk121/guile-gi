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

(define-syntax-rule (boolarray-input f)
  (test-assert (symbol->string (quote f))
    (begin
      (f #(#t #f #t #t))
      #t)))

(define-syntax-rule (boolarray-output f)
  (test-assert (symbol->string (quote f))
    (vector= eqv? (array-bool-out) #(#t #f #t #t))))

(boolarray-input array-bool-in)
(boolarray-input garray-bool-none-in)

(boolarray-output array-bool-out)

(test-assert "array-enum-in"
  ;; poor naming choice, symbol->enum is actually symbol->MarshallEnum
  (begin
    (array-enum-in (map symbol->enum '(value1 value2 value3)))
    #t))

(test-assert "array-fixed-inout"
  ((compose
    (cute list= eqv? '(2 1 0 -1) <>)
    int-vector->list
    array-fixed-inout!
    list->int-vector)
   '(-1 0 1 2)))

(define-syntax-rule (ints-input f c)
  (test-assert (symbol->string (quote f))
    (begin
      (f (c '(-1 0 1 2)))
      #t)))

(define-syntax-rule (ints-output f c)
  (test-assert (symbol->string (quote f))
    (list= eqv? '(-1 0 1 2) (c (f)))))

(ints-input array-fixed-int-in list->int-vector)
(ints-input array-fixed-short-in list->short-vector)
(ints-input array-int64-in list->s64vector)
(ints-input array-in-guint8-len list->int-vector)
(ints-input array-in-guint64-len list->int-vector)
(ints-input array-in-len-before list->int-vector)
(ints-input array-in-len-zero-terminated list->int-vector)

(ints-input garray-int-none-in list->int-vector)

(test-assert "array-uint64-in"
  (begin
    (array-uint64-in #u64(#xFFFFFFFFFFFFFFFF 0 1 2))
    #t))

(test-assert "garray-uint64-none-in"
  (begin
    (garray-uint64-none-in #u64(0 #xFFFFFFFFFFFFFFFF))
    #t))

(test-assert "garray-uint64-none-return"
  (list= eqv? (u64vector->list (garray-uint64-none-return))
         '(0 #xFFFFFFFFFFFFFFFF)))

(test-assert "array-in-nonzero-nonlen"
  (begin
    (array-in-nonzero-nonlen 1 (string->utf8 "abcd"))
    #t))

(test-assert "array-uint8-in"
  (begin
    (array-uint8-in (string->utf8 "abcd"))
    ;; (array-uint8-in #vu(97 98 99 100))
    #t))

(test-assert "array-in"
  (begin
    (array-in (list->int-vector '(-1 0 1 2)))
    #t))

(ints-output array-fixed-int-return int-vector->list)
(ints-output array-fixed-out int-vector->list)
(ints-output array-fixed-short-return short-vector->list)

(ints-output gslist-int-none-return identity)

(test-assert "array-return"
  (list= eqv? '(-1 0 1 2) (int-vector->list (array-return))))

(test-assert "array-return-etc"
  (receive (vals sum)
      (array-return-etc 2 9)
    (and (= sum 11)
         (list= eqv? '(2 0 1 9) (int-vector->list vals)))))

(test-assert "array-out"
  (list= eqv? '(-1 0 1 2) (int-vector->list (array-out))))

(test-assert "array-out-etc"
  (receive (vals sum)
      (array-out-etc 2 9)
    (and (= sum 11)
         (list= eqv? '(2 0 1 9) (int-vector->list vals)))))

(test-assert "array-inout"
  (list= eqv? '(-2 -1 0 1 2)
         (int-vector->list
          (array-inout! (list->int-vector '(-1 0 1 2))))))

(test-assert "array-inout-etc"
  (receive (vals sum)
      (array-inout-etc! -3 (list->int-vector '(-1 0 1 2)) 4)
    (and (= sum 1)
         (list= eqv? '(-3 -1 0 1 4) (int-vector->list vals)))))

;; do not do this in your production code
;; it will haunt you
(define (make-boxed-struct value)
  (let* ((struct (make <MarshallBoxedStruct>))
         (data (pointer->bytevector (slot-ref struct 'value)
                                    (sizeof long))))
    (case (sizeof long)
      ((4)
       (bytevector-s32-native-set! data 0 value))
      ((8)
       (bytevector-s64-native-set! data 0 value)))
    struct))

(test-assert "boxed struct has size slot"
  (<= 12 (class-slot-ref <MarshallBoxedStruct> 'size)))

(test-assert "array-struct-in"
  (array-struct-in (vector (make-boxed-struct 1)
                           (make-boxed-struct 2)
                           (make-boxed-struct 3))))

(test-assert "array-struct-value-in"
  (array-struct-value-in (vector (make-boxed-struct 1)
                                 (make-boxed-struct 2)
                                 (make-boxed-struct 3))))

(test-assert "boxed union has size slot"
  (<= 4 (class-slot-ref <MarshallUnion> 'size)))

(test-equal "array-zero-terminated-return-struct"
  #(42 43 44)
  (vector-map
   (lambda (_ s)
     ;; parse manually without relying on fiddle
     (car (parse-c-struct (slot-ref s 'value) (list long))))
   (array-zero-terminated-return-struct)))

(test-assert "array-gvariant-none-in"
  (let* ((v1 (variant:new-int32 27))
         (v2 (variant:new-string "Hello"))
         (y (array-gvariant-none-in (vector v1 v2))))
    (and (= 27 (variant:get-int32 (vector-ref y 0)))
         (string=? "Hello" (variant:get-string (vector-ref y 1))))))

(test-assert "array-gvariant-container-in"
  (let* ((v1 (variant:new-int32 27))
         (v2 (variant:new-string "Hello"))
         (y (array-gvariant-container-in (vector v1 v2))))
    (and (= 27 (variant:get-int32 (vector-ref y 0)))
         (string=? "Hello" (variant:get-string (vector-ref y 1))))))

(test-assert "array-gvariant-full-in"
  (let* ((v1 (variant:new-int32 27))
         (v2 (variant:new-string "Hello"))
         (y (array-gvariant-full-in (vector v1 v2))))
    (and (= 27 (variant:get-int32 (vector-ref y 0)))
         (string=? "Hello" (variant:get-string (vector-ref y 1))))))

(test-assert "array-string-in"
  (begin
    (array-string-in #("foo" "bar"))
    #t))

(define-syntax-rule (utf8-input f)
  (test-assert (symbol->string (quote f))
    (begin
      (f CONSTANT_UTF8)
      #t)))

(define-syntax-rule (utf8-inout f)
  (test-assert (symbol->string (quote f))
    (string-null? (f CONSTANT_UTF8))))


(define-syntax-rule (utf8-output f)
  (test-assert (symbol->string (quote f))
    (string=? (f) CONSTANT_UTF8)))

(utf8-input array-unichar-in)
(utf8-input garray-unichar-none-in)

(utf8-inout utf8-none-inout!)
(utf8-inout utf8-full-inout!)

(utf8-output array-unichar-out)
(utf8-output array-zero-terminated-return-unichar)
(utf8-output utf8-none-out)
(utf8-output utf8-full-out)
(utf8-output utf8-none-return)
(utf8-output utf8-full-return)

(define-syntax-rule (stringarray-input f)
  (test-assert (symbol->string (quote f))
    (begin
      (f #("0" "1" "2"))
      #t)))

(define-syntax-rule (stringarray-inout f)
  (test-assert (symbol->string (quote f))
    (vector= string=? #("-1" "0" "1" "2")
             (f #("0" "1" "2")))))

(define-syntax-rule (stringarray-inout* f)
  (test-assert (symbol->string (quote f))
    (vector= string=? #("-2" "-1" "0" "1")
             (f #("0" "1" "2")))))

(define-syntax-rule (stringarray-output f)
  (test-assert (symbol->string (quote f))
    (vector= string=? (f) #("0" "1" "2"))))

(stringarray-input array-zero-terminated-in)
(stringarray-input garray-utf8-none-in)

(stringarray-inout array-zero-terminated-inout!)
(stringarray-inout* garray-utf8-none-inout!)
(stringarray-inout* garray-utf8-container-inout!)
(stringarray-inout* garray-utf8-full-inout!)

;; 01Aug19 - This doesn't work.  The unit test modifies the GArray
;; in place, but, the GArray here is just a temp variable that
;; gets created from the vector. For caller-allocates, out GArray
;; arguments, there needs to be an additional step that modifies
;; the input vector.
;; 01Feb20 - This crashes even harder when we try to process
;; the resulting GArray.
(test-expect-fail "garray-utf8-full-out-caller-allocated")
(test-assert "garray-utf8-full-out-caller-allocated" #f
  ;; (let ((x #("A" "B" "C")))
  ;;   (garray-utf8-full-out-caller-allocated x)
  ;;   (vector= string=? #("0" "1" "2") x))
  )

(stringarray-output array-zero-terminated-out)
(stringarray-output array-zero-terminated-return)
(stringarray-output garray-utf8-none-out)
(stringarray-output garray-utf8-none-return)
(stringarray-output garray-utf8-container-out)
(stringarray-output garray-utf8-container-return)
(stringarray-output garray-utf8-full-out)
(stringarray-output garray-utf8-full-return)

(stringarray-output gptrarray-utf8-none-return)
(stringarray-output gptrarray-utf8-container-return)
(stringarray-output gptrarray-utf8-full-return)
(stringarray-output gptrarray-utf8-none-out)
(stringarray-output gptrarray-utf8-container-out)
(stringarray-output gptrarray-utf8-full-out)

(test-assert "bytearray-none-in"
  (bytearray-none-in #vu8(0 49 255 51)))

(test-assert "gbytes-none-in"
  (gbytes-none-in (bytes:new-take #vu8(0 49 255 51))))

(test-equal "bytearray-full-return"
  #vu8(0 49 255 51)
  (bytearray-full-return))

(test-equal "gbytes-full-return"
  #vu8(0 49 255 51)
  (bytes:unref-to-array (gbytes-full-return)))

(define-syntax-rule (stringlist-output f)
  (test-assert (symbol->string (quote f))
    (list= string=? (f) '("0" "1" "2"))))

(stringlist-output gslist-utf8-none-return)
(stringlist-output gslist-utf8-container-return)
(stringlist-output gslist-utf8-full-return)

(test-assert "array-zero-terminated-return-null"
  (vector-empty? (array-zero-terminated-return-null)))

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

(test-assert "init-function-null"
  (init-function!))

(test-assert "init-function-one"
  (receive (ret butlast)
      (init-function! #("--help"))
    (and ret (not butlast))))

(test-assert "init-function-two"
  (receive (ret butlast)
      (init-function! #("--help" "--verbose"))
    (and (vector= string= butlast #("--help"))
         ret)))


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


(test-assert "utf8-as-uint8array-in"
  (begin
    (utf8-as-uint8array-in (string->utf8 CONSTANT_UTF8))
    #t))

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
  ;; skip value/closure tests -- they are broken
  (test-skip 16)
  ;; These tests weirdly pass:
  ;;   - gvalue-flat-array
  ;;   - gvalue-flat-array-round-trip
  ;;   - gvalue-multi-array-key-value-in
  ;;   - gvalue-return->in
  ;; These tests error:
  ;;   return-gvalue-flat-array:
  ;;   GuileGI-ERROR **: 13:55:16.003: unhandled argument type 'pointer to GArray of type GBoxed' src/gig_argument.c:1542
  )

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

(test-assert "gvalue-flat-array"
  (begin
    (gvalue-flat-array
     (list->vector
      (map make-value
           (list G_TYPE_INT <string> G_TYPE_BOOLEAN)
           '(42 "42" #t))))
    #t))

(test-equal "return-gvalue-flat-array"
  '(42 "42" #t)
  (map (cute <>) (vector->list (return-gvalue-flat-array))))

;; This test requires GValues to be passed by value on the C stack, which is not
;; supported.
(test-expect-fail "gvalue-flat-array-round-trip")
(test-equal "gvalue-flat-array-round-trip"
  '(42 "42" #t)
  (map (cute <>) (vector->list (gvalue-flat-array-round-trip
                                (make-value G_TYPE_INT 42)
                                (make-value <string> "42")
                                (make-value G_TYPE_BOOLEAN #t)))))

(test-assert "multi-array-key-value-in"
  (begin
    (multi-array-key-value-in
     #("one" "two" "three")
     (list->vector (map (cute make-value G_TYPE_INT <>) '(1 2 3))))
    #t))

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
