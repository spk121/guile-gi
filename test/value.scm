(use-modules (gi)
             (gi types)
             (gi repository) (srfi srfi-64) (oop goops)
             (srfi srfi-26)
             (ice-9 hash-table)
             (ice-9 receive)
             (system foreign))

(require "GLib" "2.0")
(load-by-name "GLib" "MainLoop")
(test-begin "value")

(define (value-passthrough type val)
  (let ((value (make <GValue>)))
    (set! (value type) val)
    (value)))

(test-equal "getset boolean"
  #t
  (value-passthrough G_TYPE_BOOLEAN #t))

(test-error "getset boolean wrong type"
  #t
  (value-passthrough G_TYPE_BOOLEAN 'foo))

(test-equal "getset char"
  42
  (value-passthrough G_TYPE_CHAR 42))

(test-error "getset char wrong type"
  #t
  (value-passthrough G_TYPE_CHAR 'foo))

(test-error "getset char out of range"
  #t
  (value-passthrough G_TYPE_CHAR (expt 2 9)))

(test-equal "getset uchar"
  42
  (value-passthrough G_TYPE_UCHAR 42))

(test-error "getset uchar wrong type"
  #t
  (value-passthrough G_TYPE_UCHAR 'foo))

(test-error "getset uchar out of range"
  #t
  (value-passthrough G_TYPE_UCHAR -1))

(test-equal "getset int"
  42
  (value-passthrough G_TYPE_INT 42))

(test-error "getset int wrong type"
  #t
  (value-passthrough G_TYPE_INT 'foo))

(test-error "getset int out of range"
  #t
  (value-passthrough G_TYPE_INT (expt 2 65)))

(test-equal "getset uint"
  42
  (value-passthrough G_TYPE_UINT 42))

(test-error "getset uint wrong type"
  #t
  (value-passthrough G_TYPE_UINT 'foo))

(test-error "getset uint out of range"
  #t
  (value-passthrough G_TYPE_UINT -1))

(test-equal "getset long"
  42
  (value-passthrough G_TYPE_LONG 42))

(test-error "getset long wrong type"
  #t
  (value-passthrough G_TYPE_LONG 'foo))

(test-error "getset long out of range"
  #t
  (value-passthrough G_TYPE_LONG (expt 2 65)))

(test-equal "getset ulong"
  42
  (value-passthrough G_TYPE_ULONG 42))

(test-error "getset ulong wrong type"
  #t
  (value-passthrough G_TYPE_ULONG 'foo))

(test-error "getset ulong out of range"
  #t
  (value-passthrough G_TYPE_ULONG -1))

(test-equal "getset int64"
  42
  (value-passthrough G_TYPE_INT64 42))

(test-error "getset int64 wrong type"
  #t
  (value-passthrough G_TYPE_INT64 'foo))

(test-error "getset int64 out of range"
  #t
  (value-passthrough G_TYPE_INT64 (expt 2 65)))

(test-equal "getset uint64"
  42
  (value-passthrough G_TYPE_UINT64 42))

(test-error "getset uint64 wrong type"
  #t
  (value-passthrough G_TYPE_UINT64 'foo))

(test-error "getset uint64 out of range"
  #t
  (value-passthrough G_TYPE_UINT64 -1))

(test-approximate "getset float"
  1234.5678
  (value-passthrough G_TYPE_FLOAT 1234.5678)
  0.0001)

(test-error "getset float wrong type"
  #t
  (value-passthrough G_TYPE_FLOAT 'foo))

(test-error "getset float out of range"
  #t
  (value-passthrough G_TYPE_FLOAT 1.0e40))

(test-approximate "getset double"
  1234.5678
  (value-passthrough G_TYPE_DOUBLE 1234.5678)
  0.0001)

(test-error "getset double wrong type"
  #t
  (value-passthrough G_TYPE_DOUBLE 'foo))

(test-equal "getset ASCII string"
  "hello"
  (value-passthrough <string> "hello"))

(test-equal "getset unicode string"
  "红掌拨清波"
  (value-passthrough <string> "红掌拨清波"))

(test-error "getset string wrong type"
  #t
  (value-passthrough <string> 'foo))

(let ((ptr (bytevector->pointer (u32vector 1 2 3))))
  (test-equal "getset pointer"
    ptr
    (value-passthrough G_TYPE_POINTER ptr)))

(define-class <Flags> (<GFlags>))
(class-slot-set! <Flags> 'obarray
                 (alist->hashq-table
                  '((alpha . 1)
                    (bravo . 2)
                    (charlie . 4)
                    (delta . 8)
                    (echo . 16))))
(define flags1 (list->flags <Flags> '(charlie delta)))

(test-expect-fail "getset flags")
(test-equal "getset flags"
  flags1

  (let ((value (make <GValue>)))
    (set! (value G_TYPE_FLAGS) flags1)
    (value)))


(test-error "getset flags wrong type"
  #t
  (value-passthrough G_TYPE_FLAGS 4))

(define-class <Enum> (<GEnum>))
(class-slot-set! <Enum> 'obarray
                 (alist->hashq-table
                  '((foxtrot . 1)
                    (golf . 2)
                    (hotel . 3)
                    (india . 4)
                    (juliette . 5))))
(define enum1 (symbol->enum <Enum> 'hotel))

(test-expect-fail "getset enum")
(test-equal "getset enum"
  enum1
  (value-passthrough G_TYPE_ENUM enum1))

(test-error "getset enum wrong type"
  #t
  (value-passthrough G_TYPE_ENUM flags1))

(test-expect-fail 1)
(test-assert "getset GLib object"
  (let* ((obj (make <GObject>))
         (obj2 (value-passthrough G_TYPE_OBJECT obj)))
    (eqv? obj obj2)))

(test-equal "transform"
  "42"
  (let ((value (make <GValue>)))
    (set! (value G_TYPE_INT) 42)
    ((transform value <string>))))

(define (int->value i)
  (let ((v (make <GValue>)))
    (set! (v G_TYPE_INT) i)
    v))

(test-equal "simple-closure"
  42
  ((apply (procedure->closure *)
          G_TYPE_INT
          (map int->value '(21 2)))))

(test-equal "inout-closure"
  '(42 42 42)
  (map (cute <>)
       (call-with-values
           (lambda ()
             (apply (procedure->closure
                     (lambda (a b)
                       (let ((c (* a b)))
                         (values c c c)))
                     #*11)
                    (cons G_TYPE_INT #*11)
                    (map int->value '(21 2))))
         list)))

(test-end "value")
