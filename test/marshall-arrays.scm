;;; Every function of GIMarshallingTests that takes, returns or outputs
;;; an array or a list: C arrays (of fixed size, zero-terminated, and
;;; with a length argument), GArray, GPtrArray, GByteArray, GBytes,
;;; GStrv, GList and GSList.
;;;
;;; The last tests read Marshall-1.0.gir and check that this file called
;;; every such function the library has, so that a function this file
;;; leaves out, or one that a later version of the library adds, fails
;;; the test instead of going unnoticed.
;;;
;;; The C functions check their arguments with g_assert, so a wrong
;;; argument ends the test with an error.

(use-modules (ice-9 receive)
             (oop goops)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-43)
             (srfi srfi-64)
             (system foreign)
             (gi)
             (gi types)
             (gi util)
             (test gir))

(test-begin "marshall-arrays")

;; We know, that this Marshall exists, because we don't run the test
;; if it does not. GLib is an explicit dependency, so it too always exists
(use-typelibs (("Marshall" "1.0")
               #:renamer (protect* '(sizeof short int long size_t)))
              (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip int))))

;; The functions that get called, by name, for the tests at the end.
(define called (make-hash-table))

(add-hook! %before-function-hook
           (lambda (name args)
             (hash-set! called (strip-suffix name) #t)))

;; Call a function whose C side checks what it is given.
(define-syntax-rule (test-call name expr)
  (test-assert name (begin expr #t)))

(define-syntax-rule (values->list expr)
  (call-with-values (lambda () expr) list))

;; A MarshallBoxedStruct whose first field, a long, is VALUE.
(define (make-boxed-struct value)
  (let* ((struct (make <MarshallBoxedStruct>))
         (data (pointer->bytevector (slot-ref struct 'value) (sizeof long))))
    (case (sizeof long)
      ((4) (bytevector-s32-native-set! data 0 value))
      ((8) (bytevector-s64-native-set! data 0 value)))
    struct))

(define (boxed-struct-long struct)
  (car (parse-c-struct (slot-ref struct 'value) (list long))))

(define (make-value type value)
  (let ((v (make <GValue>)))
    (set! (v type) value)
    v))

(define ints '(-1 0 1 2))
(define strings #("0" "1" "2"))
(define bytes #vu8(0 49 255 51))

;;; C arrays of a fixed size

(test-call "array-fixed-int-in" (array-fixed-int-in (list->int-vector ints)))

(test-error "array-fixed-int-in with too few elements"
  ;; The C function reads four ints whatever it is given.
  #t
  (array-fixed-int-in (list->int-vector '(1 2))))

(test-call "array-fixed-short-in" (array-fixed-short-in (list->short-vector ints)))

(test-equal "array-fixed-int-return"
  ints (int-vector->list (array-fixed-int-return)))

(test-equal "array-fixed-short-return"
  ints (short-vector->list (array-fixed-short-return)))

(test-equal "array-fixed-out"
  ints (int-vector->list (array-fixed-out)))

(test-equal "array-fixed-inout"
  '(2 1 0 -1)
  (int-vector->list (array-fixed-inout! (list->int-vector ints))))

(test-equal "array-fixed-caller-allocated-out"
  ;; The caller's array is filled in, and returned.
  (list ints ints)
  (let* ((array (list->int-vector '(9 9 9 9)))
         (result (array-fixed-caller-allocated-out! array)))
    (list (int-vector->list array) (int-vector->list result))))

(test-equal "array-fixed-out-uninitialized"
  ;; A function that fails leaves its output unset.
  '(#f ())
  (receive (ok array) (array-fixed-out-uninitialized)
    (list ok (int-vector->list array))))

;;; C arrays with a length argument

(test-call "array-in" (array-in (list->int-vector ints)))
(test-call "array-in-len-before" (array-in-len-before (list->int-vector ints)))
(test-call "array-in-len-zero-terminated"
  (array-in-len-zero-terminated (list->int-vector ints)))
(test-call "array-in-guint64-len" (array-in-guint64-len (list->int-vector ints)))
(test-call "array-in-guint8-len" (array-in-guint8-len (list->int-vector ints)))
(test-call "array-int64-in" (array-int64-in (list->s64vector ints)))
(test-call "array-uint64-in" (array-uint64-in #u64(#xFFFFFFFFFFFFFFFF 0 1 2)))
(test-call "array-uint8-in" (array-uint8-in (string->utf8 "abcd")))
(test-call "array-in-nonzero-nonlen"
  (array-in-nonzero-nonlen 1 (string->utf8 "abcd")))
(test-call "utf8-as-uint8array-in"
  (utf8-as-uint8array-in (string->utf8 CONSTANT_UTF8)))
(test-call "array-bool-in" (array-bool-in #(#t #f #t #t)))
(test-call "array-unichar-in" (array-unichar-in CONSTANT_UTF8))
(test-call "array-string-in" (array-string-in #("foo" "bar")))

(test-call "array-enum-in"
  ;; symbol->enum is the conversion of the one enum that defines it.
  (array-enum-in (map symbol->enum '(value1 value2 value3))))

(test-call "array-flags-in"
  (array-flags-in (list->vector
                   (map (compose list->flags list) '(value1 value2 value3)))))

(test-call "array-in-utf8-two-in"
  (array-in-utf8-two-in (list->int-vector ints) "1" "2"))

(test-call "array-in-utf8-two-in, without the optional strings"
  (array-in-utf8-two-in (list->int-vector ints)))

(test-call "array-in-utf8-two-in-out-of-order"
  (array-in-utf8-two-in-out-of-order "1" (list->int-vector ints) "2"))

(test-equal "array-return"
  ints (int-vector->list (array-return)))

(test-equal "array-return-etc"
  '((2 0 1 9) 11)
  (receive (array sum) (array-return-etc 2 9)
    (list (int-vector->list array) sum)))

(test-equal "array-out"
  ints (int-vector->list (array-out)))

(test-equal "array-out-etc"
  '((2 0 1 9) 11)
  (receive (array sum) (array-out-etc 2 9)
    (list (int-vector->list array) sum)))

(test-equal "array-out-uninitialized"
  '(#f ())
  (receive (ok array) (array-out-uninitialized)
    (list ok (int-vector->list array))))

(test-equal "array-inout"
  '(-2 -1 0 1 2)
  (int-vector->list (array-inout! (list->int-vector ints))))

(test-equal "array-inout-etc"
  '((-3 -1 0 1 4) 1)
  (receive (array sum) (array-inout-etc! -3 (list->int-vector ints) 4)
    (list (int-vector->list array) sum)))

(test-equal "array-bool-out"
  #(#t #f #t #t) (array-bool-out))

(test-equal "array-unichar-out"
  CONSTANT_UTF8 (array-unichar-out))

(test-assert "init-function, without arguments"
  (car (values->list (init-function!))))

(test-equal "init-function, one argument"
  '(#t #f)
  (values->list (init-function! #("--help"))))

(test-equal "init-function, two arguments"
  '(#t #("--help"))
  (values->list (init-function! #("--help" "--verbose"))))

(test-call "multi-array-key-value-in"
  (multi-array-key-value-in
   #("one" "two" "three")
   (list->vector (map (cute make-value G_TYPE_INT <>) '(1 2 3)))))

(test-equal "gerror-array-in"
  ;; The array argument does not get in the way of the GError.
  CONSTANT_GERROR_MESSAGE
  (catch #t
    (lambda () (gerror-array-in (list->int-vector '(1 2 3 0))) #f)
    (lambda (key subr message args . rest) (car args))))

;;; Methods

(define object (make <MarshallObject> #:int 42))

(test-call "object:method-array-in"
  (object:method-array-in object (list->int-vector ints)))

(test-equal "object:method-array-out"
  ints (int-vector->list (object:method-array-out object)))

(test-equal "object:method-array-inout"
  '(-2 -1 0 1 2)
  (int-vector->list (object:method-array-inout! object (list->int-vector ints))))

(test-equal "object:method-array-return"
  ints (int-vector->list (object:method-array-return object)))

(test-equal "object:vfunc-array-out-parameter"
  ;; The method calls a virtual function, here one written in Scheme.
  '(1.0 2.0 3.0)
  (let ((<Sub> (register-type
                "MarshallArraysSub" <MarshallObject>
                #:vfuncs
                `((vfunc-array-out-parameter . ,(lambda (self) #f32(1.0 2.0 3.0)))))))
    (f32vector->list (object:vfunc-array-out-parameter (make <Sub>)))))

(test-equal "interface3:test-variant-array-in"
  ;; The library has the interface but no implementation of it.
  '(27 "Hello")
  (let* ((received #f)
         (<Impl> (register-type
                  "MarshallArraysInterface3" <GObject>
                  #:interfaces (list <MarshallInterface3>)
                  #:vfuncs
                  `((test-variant-array-in
                     . ,(lambda (self variants)
                          (set! received
                            (list (variant:get-int32 (vector-ref variants 0))
                                  (variant:get-string (vector-ref variants 1))))))))))
    (interface3:test-variant-array-in
     (make <Impl>)
     (vector (variant:new-int32 27) (variant:new-string "Hello")))
    received))

;;; Zero-terminated C arrays

(test-call "array-zero-terminated-in" (array-zero-terminated-in strings))

(test-equal "array-zero-terminated-out"
  strings (array-zero-terminated-out))

(test-equal "array-zero-terminated-return"
  strings (array-zero-terminated-return))

(test-equal "array-zero-terminated-inout"
  #("-1" "0" "1" "2") (array-zero-terminated-inout! strings))

(test-equal "array-zero-terminated-return-null"
  #() (array-zero-terminated-return-null))

(test-equal "array-zero-terminated-return-unichar"
  CONSTANT_UTF8 (array-zero-terminated-return-unichar))

(test-equal "array-zero-terminated-out-uninitialized"
  '(#f #())
  (values->list (array-zero-terminated-out-uninitialized)))

(test-call "gstrv-in" (gstrv-in strings))

(test-equal "gstrv-return"
  strings (gstrv-return))

(test-equal "gstrv-out"
  strings (gstrv-out))

(test-equal "gstrv-inout"
  #("-1" "0" "1" "2") (gstrv-inout! strings))

(test-equal "gstrv-out-uninitialized"
  '(#f #())
  (values->list (gstrv-out-uninitialized)))

;;; C arrays of structs, variants and values

(test-assert "boxed struct has size slot"
  (<= 12 (class-slot-ref <MarshallBoxedStruct> 'size)))

(test-assert "boxed union has size slot"
  (<= 4 (class-slot-ref <MarshallUnion> 'size)))

(test-call "array-struct-in"
  ;; an array of pointers to structs
  (array-struct-in (list->vector (map make-boxed-struct '(1 2 3)))))

(test-call "array-struct-value-in"
  ;; an array of structs
  (array-struct-value-in (list->vector (map make-boxed-struct '(1 2 3)))))

(test-equal "array-struct-take-in"
  ;; The callee frees the array and its structs; ours are still good.
  '(1 2 3)
  (let ((structs (map make-boxed-struct '(1 2 3))))
    (array-struct-take-in (list->vector structs))
    (gc)
    (map boxed-struct-long structs)))

(test-equal "array-zero-terminated-return-struct"
  '(42 43 44)
  (map boxed-struct-long (vector->list (array-zero-terminated-return-struct))))

(define (variants)
  (vector (variant:new-int32 27) (variant:new-string "Hello")))

(define (variants->list variants)
  (list (variant:get-int32 (vector-ref variants 0))
        (variant:get-string (vector-ref variants 1))))

(test-equal "array-gvariant-none-in"
  '(27 "Hello") (variants->list (array-gvariant-none-in (variants))))

(test-equal "array-gvariant-container-in"
  '(27 "Hello") (variants->list (array-gvariant-container-in (variants))))

(test-equal "array-gvariant-full-in"
  '(27 "Hello") (variants->list (array-gvariant-full-in (variants))))

(test-call "gvalue-flat-array"
  (gvalue-flat-array
   (list->vector
    (map make-value (list G_TYPE_INT <string> G_TYPE_BOOLEAN) '(42 "42" #t)))))

(test-equal "return-gvalue-flat-array"
  '(42 "42" #t)
  (map (cute <>) (vector->list (return-gvalue-flat-array))))

(test-equal "return-gvalue-zero-terminated-array"
  '(42 "42" #t)
  (map (cute <>) (vector->list (return-gvalue-zero-terminated-array))))

;; This takes three GValues by value, on the C stack, which is not
;; supported.
(test-expect-fail "gvalue-flat-array-round-trip")
(test-equal "gvalue-flat-array-round-trip"
  '(42 "42" #t)
  (map (cute <>) (vector->list (gvalue-flat-array-round-trip
                                (make-value G_TYPE_INT 42)
                                (make-value <string> "42")
                                (make-value G_TYPE_BOOLEAN #t)))))

;;; GArray

(test-call "garray-int-none-in" (garray-int-none-in (list->int-vector ints)))
(test-call "garray-uint64-none-in"
  (garray-uint64-none-in #u64(0 #xFFFFFFFFFFFFFFFF)))
(test-call "garray-bool-none-in" (garray-bool-none-in #(#t #f #t #t)))
(test-call "garray-unichar-none-in" (garray-unichar-none-in CONSTANT_UTF8))
(test-call "garray-utf8-none-in" (garray-utf8-none-in strings))

(test-equal "garray-int-none-return"
  ints (int-vector->list (garray-int-none-return)))

(test-equal "garray-int-none-return twice"
  ;; The array belongs to the callee, and must outlive the call.
  (list ints ints)
  (list (int-vector->list (garray-int-none-return))
        (int-vector->list (garray-int-none-return))))

(test-equal "garray-uint64-none-return"
  '(0 #xFFFFFFFFFFFFFFFF) (u64vector->list (garray-uint64-none-return)))

(test-equal "garray-utf8-none-return" strings (garray-utf8-none-return))
(test-equal "garray-utf8-container-return" strings (garray-utf8-container-return))
(test-equal "garray-utf8-full-return" strings (garray-utf8-full-return))

(test-equal "garray-utf8-none-out" strings (garray-utf8-none-out))
(test-equal "garray-utf8-container-out" strings (garray-utf8-container-out))
(test-equal "garray-utf8-full-out" strings (garray-utf8-full-out))

(test-equal "garray-utf8-full-out-caller-allocated"
  ;; The callee empties and fills an array the caller made.
  strings
  (garray-utf8-full-out-caller-allocated! (vector "A" "B")))

(test-equal "garray-utf8-none-inout"
  #("-2" "-1" "0" "1") (garray-utf8-none-inout! strings))
(test-equal "garray-utf8-container-inout"
  #("-2" "-1" "0" "1") (garray-utf8-container-inout! strings))
(test-equal "garray-utf8-full-inout"
  #("-2" "-1" "0" "1") (garray-utf8-full-inout! strings))

(test-equal "garray-utf8-none-out-uninitialized"
  '(#f #()) (values->list (garray-utf8-none-out-uninitialized)))
(test-equal "garray-utf8-container-out-uninitialized"
  '(#f #()) (values->list (garray-utf8-container-out-uninitialized)))
(test-equal "garray-utf8-full-out-uninitialized"
  '(#f #()) (values->list (garray-utf8-full-out-uninitialized)))

(test-equal "garray-boxed-struct-full-return"
  ;; structs stored by value
  '(42 43 44)
  (map boxed-struct-long (vector->list (garray-boxed-struct-full-return))))

;;; GPtrArray

(test-call "gptrarray-utf8-none-in" (gptrarray-utf8-none-in strings))

(test-equal "gptrarray-utf8-none-return" strings (gptrarray-utf8-none-return))

(test-equal "gptrarray-utf8-none-return twice"
  ;; The callee's (static) array used to be freed after the first call.
  (list strings strings)
  (list (gptrarray-utf8-none-return) (gptrarray-utf8-none-return)))

(test-equal "gptrarray-utf8-container-return"
  strings (gptrarray-utf8-container-return))
(test-equal "gptrarray-utf8-full-return" strings (gptrarray-utf8-full-return))

(test-equal "gptrarray-utf8-none-out" strings (gptrarray-utf8-none-out))
(test-equal "gptrarray-utf8-container-out" strings (gptrarray-utf8-container-out))
(test-equal "gptrarray-utf8-full-out" strings (gptrarray-utf8-full-out))

(test-equal "gptrarray-utf8-none-inout"
  #("-2" "-1" "0" "1") (gptrarray-utf8-none-inout! strings))
(test-equal "gptrarray-utf8-container-inout"
  #("-2" "-1" "0" "1") (gptrarray-utf8-container-inout! strings))
(test-equal "gptrarray-utf8-full-inout"
  #("-2" "-1" "0" "1") (gptrarray-utf8-full-inout! strings))

(test-equal "gptrarray-utf8-none-out-uninitialized"
  '(#f #()) (values->list (gptrarray-utf8-none-out-uninitialized)))
(test-equal "gptrarray-utf8-container-out-uninitialized"
  '(#f #()) (values->list (gptrarray-utf8-container-out-uninitialized)))
(test-equal "gptrarray-utf8-full-out-uninitialized"
  '(#f #()) (values->list (gptrarray-utf8-full-out-uninitialized)))

(test-equal "gptrarray-boxed-struct-full-return"
  ;; pointers to structs
  '(42 43 44)
  (map boxed-struct-long (vector->list (gptrarray-boxed-struct-full-return))))

;;; GByteArray and GBytes

(test-call "bytearray-none-in" (bytearray-none-in bytes))

(test-equal "bytearray-full-return" bytes (bytearray-full-return))

(test-call "gbytes-none-in" (gbytes-none-in (bytes:new-take bytes)))

(test-equal "gbytes-full-return"
  bytes (bytes:unref-to-array (gbytes-full-return)))

;;; GList

(test-call "glist-int-none-in" (glist-int-none-in ints))
(test-call "glist-uint32-none-in" (glist-uint32-none-in '(0 #xFFFFFFFF)))
(test-call "glist-utf8-none-in" (glist-utf8-none-in '("0" "1" "2")))

(test-error "glist-int-none-in with a vector"
  #t
  (glist-int-none-in (list->vector ints)))

(test-equal "glist-int-none-return" ints (glist-int-none-return))
(test-equal "glist-uint32-none-return" '(0 #xFFFFFFFF) (glist-uint32-none-return))

(test-equal "glist-utf8-none-return" '("0" "1" "2") (glist-utf8-none-return))
(test-equal "glist-utf8-container-return"
  '("0" "1" "2") (glist-utf8-container-return))
(test-equal "glist-utf8-full-return" '("0" "1" "2") (glist-utf8-full-return))

(test-equal "glist-utf8-none-out" '("0" "1" "2") (glist-utf8-none-out))
(test-equal "glist-utf8-container-out" '("0" "1" "2") (glist-utf8-container-out))
(test-equal "glist-utf8-full-out" '("0" "1" "2") (glist-utf8-full-out))

(test-equal "glist-utf8-none-inout"
  '("-2" "-1" "0" "1") (glist-utf8-none-inout! '("0" "1" "2")))
(test-equal "glist-utf8-container-inout"
  '("-2" "-1" "0" "1") (glist-utf8-container-inout! '("0" "1" "2")))
(test-equal "glist-utf8-full-inout"
  '("-2" "-1" "0" "1") (glist-utf8-full-inout! '("0" "1" "2")))

(test-equal "glist-utf8-none-out-uninitialized"
  '(#f ()) (values->list (glist-utf8-none-out-uninitialized)))
(test-equal "glist-utf8-container-out-uninitialized"
  '(#f ()) (values->list (glist-utf8-container-out-uninitialized)))
(test-equal "glist-utf8-full-out-uninitialized"
  '(#f ()) (values->list (glist-utf8-full-out-uninitialized)))

;;; GSList

(test-call "gslist-int-none-in" (gslist-int-none-in ints))
(test-call "gslist-utf8-none-in" (gslist-utf8-none-in '("0" "1" "2")))

(test-equal "gslist-int-none-return" ints (gslist-int-none-return))

(test-equal "gslist-utf8-none-return" '("0" "1" "2") (gslist-utf8-none-return))
(test-equal "gslist-utf8-container-return"
  '("0" "1" "2") (gslist-utf8-container-return))
(test-equal "gslist-utf8-full-return" '("0" "1" "2") (gslist-utf8-full-return))

(test-equal "gslist-utf8-none-out" '("0" "1" "2") (gslist-utf8-none-out))
(test-equal "gslist-utf8-container-out" '("0" "1" "2") (gslist-utf8-container-out))
(test-equal "gslist-utf8-full-out" '("0" "1" "2") (gslist-utf8-full-out))

(test-equal "gslist-utf8-none-inout"
  '("-2" "-1" "0" "1") (gslist-utf8-none-inout! '("0" "1" "2")))
(test-equal "gslist-utf8-container-inout"
  '("-2" "-1" "0" "1") (gslist-utf8-container-inout! '("0" "1" "2")))
(test-equal "gslist-utf8-full-inout"
  '("-2" "-1" "0" "1") (gslist-utf8-full-inout! '("0" "1" "2")))

(test-equal "gslist-utf8-none-out-uninitialized"
  '(#f ()) (values->list (gslist-utf8-none-out-uninitialized)))
(test-equal "gslist-utf8-container-out-uninitialized"
  '(#f ()) (values->list (gslist-utf8-container-out-uninitialized)))
(test-equal "gslist-utf8-full-out-uninitialized"
  '(#f ()) (values->list (gslist-utf8-full-out-uninitialized)))

(test-assert "filename-list-return"
  ;; The library returns no file names.
  (null? (filename-list-return)))

;;; Lists and arrays survive a collection

(test-equal "results don't depend on what the callee freed"
  (list strings '("0" "1" "2") ints)
  (let ((from-array (garray-utf8-full-return))
        (from-list (glist-utf8-full-return))
        (from-c-array (int-vector->list (array-return))))
    (gc)
    (list from-array from-list from-c-array)))

;;; Completeness

;; The array and list types of GIR, other than the C arrays, which it
;; describes with an element of their own.
(define array-types
  '("GLib.Array" "GLib.PtrArray" "GLib.ByteArray" "GLib.Bytes"
    "GLib.Strv" "GLib.List" "GLib.SList"))

;; Functions that guile-gi doesn't bind, and why.  If one of them gets
;; bound, it needs a test above.
(define unbound
  ;; MarshallSimpleStruct has no GType, and guile-gi only knows the
  ;; layouts of such structs for the few it has been taught.
  '("array-simple-struct-in"
    "array-fixed-out-struct"
    "array-fixed-caller-allocated-struct-out"))

(define gir (find-gir "Marshall" "1.0"))

(test-assert "Marshall-1.0.gir was found" gir)

(define array-callables
  (if gir
      (filter (cut callable-mentions-type? <> array-types) (gir-callables gir))
      '()))

(format #t "Marshall: ~a functions and methods with arrays or lists in the GIR, ~a called~%"
        (length array-callables)
        (count (lambda (callable)
                 (hash-ref called (callable-scheme-name callable)))
               array-callables))

(test-assert "the GIR lists functions with arrays"
  (> (length array-callables) 100))

;; Without Guile's hooks nothing is recorded.
(when (zero? (hash-count (const #t) called))
  (test-skip 1))

(test-equal "every function with an array or a list was called"
  '()
  (filter-map
   (lambda (callable)
     (let ((name (callable-scheme-name callable)))
       (and (not (hash-ref called name))
            (not (member name unbound))
            name)))
   array-callables))

(test-equal "the functions listed as not bound are not bound"
  '()
  (filter (lambda (name)
            (any (lambda (callable)
                   (and (string=? name (callable-scheme-name callable))
                        (find-binding (current-module) callable)))
                 array-callables))
          unbound))

(test-end "marshall-arrays")
