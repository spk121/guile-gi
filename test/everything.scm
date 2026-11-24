;;; Tests of the Everything library, generated from its GIR.
;;;
;;; Everything is the library that gobject-introspection generates from
;;; a list of types: for each type it has a function that returns it, one
;;; that takes it, one that outputs it and one that passes it through.
;;; This walks the functions in Everything-1.0.gir and calls each one
;;; with values chosen by the types the GIR gives its arguments, so that
;;; nothing guile-gi fails to bind, or binds differently, is left out.

(use-modules (gi) (gi types)
             (test gir)
             (ice-9 match)
             (rnrs bytevectors)
             (system foreign)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "everything.scm")

;; We know, that this typelib exists, because we don't run the test
;; if it does not
(use-typelibs ("Everything" "1.0"))

(define (signed-samples bits)
  (list 0 1 -1 (1- (expt 2 (1- bits))) (- (expt 2 (1- bits)))))

(define (unsigned-samples bits)
  (list 0 1 (1- (expt 2 bits))))

(define (bits-of type)
  (* 8 (sizeof type)))

;; What Everything's functions return and output for each GIR type, and
;; values to pass in.  Each sample is a pair of the value to pass and
;; the value that should come back from a function that returns its
;; argument.
(define (type-zero type)
  (match type
    ((or "gfloat" "gdouble") 0.0)
    ("gboolean" #f)
    ("gunichar" #\nul)
    ((or "utf8" "filename") "")
    ("gpointer" #f)
    (_ 0)))

;; The one exception: const_return_GType returns G_TYPE_OBJECT, while
;; one_outparam_GType outputs 0, which is no type.
(define (type-return type)
  (match type
    ("GType" <GObject>)
    (_ (type-zero type))))

(define (same . values)
  (map (lambda (value) (cons value value)) values))

(define (type-samples type)
  (match type
    ("gchar" (cons (cons #\A 65) (apply same (signed-samples 8))))
    ("gint8" (apply same (signed-samples 8)))
    ("guint8" (apply same (unsigned-samples 8)))
    ("gint16" (apply same (signed-samples 16)))
    ("guint16" (apply same (unsigned-samples 16)))
    ("gint32" (apply same (signed-samples 32)))
    ("guint32" (apply same (unsigned-samples 32)))
    ("gint64" (apply same (signed-samples 64)))
    ("guint64" (apply same (unsigned-samples 64)))
    ("gshort" (apply same (signed-samples (bits-of short))))
    ("gushort" (apply same (unsigned-samples (bits-of unsigned-short))))
    ("gint" (apply same (signed-samples (bits-of int))))
    ("guint" (apply same (unsigned-samples (bits-of unsigned-int))))
    ("glong" (apply same (signed-samples (bits-of long))))
    ("gulong" (apply same (unsigned-samples (bits-of unsigned-long))))
    ("gssize" (apply same (signed-samples (bits-of ssize_t))))
    ("gsize" (apply same (unsigned-samples (bits-of size_t))))
    ("gintptr" (apply same (signed-samples (bits-of intptr_t))))
    ("guintptr" (apply same (unsigned-samples (bits-of uintptr_t))))
    ;; Their sizes vary between platforms; these fit everywhere.
    ((or "off_t" "time_t") (same 0 1 (1- (expt 2 31))))
    ("gfloat" (same 0.0 1.5 -0.25))
    ("gdouble" (same 0.0 1.5 -0.25 1e300))
    ("gboolean" (same #t #f))
    ("gunichar" (same #\A #\あ))
    ("utf8" (same "" "hello" "Việt Nam"))
    ("filename" (same "temp.txt"))
    ("gpointer" (same (bytevector->pointer (make-bytevector 8 0))))
    ("GType" (list (cons G_TYPE_OBJECT <GObject>) (cons <GObject> <GObject>)))
    (_ #f)))

(define n-functions 0)
(define n-unbound 0)
(define n-unrecognized 0)

;; Test one function.  What to expect of it follows from its shape.
(define (test-function callable)
  (define name (callable-name callable))
  (define binding (find-binding (current-module) callable))
  (define proc (and binding (module-ref (current-module) binding)))
  (define return (value-type (callable-return callable)))
  (define parameters (callable-parameters callable))
  (define (unrecognized why)
    (set! n-unrecognized (1+ n-unrecognized))
    (test-assert (string-append name ": " why) #f))

  (set! n-functions (1+ n-functions))
  (cond
   ((not proc)
    (set! n-unbound (1+ n-unbound))
    (test-assert (string-append name " is bound") #f))
   (else
    (match (map (lambda (p) (list (value-direction p) (value-type p))) parameters)
      ;; Nothing in, nothing out.
      (()
       (if (string=? return "none")
           (test-assert name (begin (proc) #t))
           (test-equal name (type-return return) (proc))))
      ;; An argument, which may be returned.
      ((('in type))
       (let ((samples (type-samples type)))
         (cond
          ((not samples)
           (unrecognized (string-append "no values for type " type)))
          ((string=? return "none")
           (test-assert name
             (begin (for-each (lambda (sample) (proc (car sample))) samples)
                    #t)))
          ((string=? return type)
           (test-equal name
             (map cdr samples)
             (map (lambda (sample) (proc (car sample))) samples)))
          (else
           (unrecognized "returns another type than it takes")))))
      ;; An output.
      ((('out type))
       (if (string=? return "none")
           (test-equal name (type-zero type) (proc))
           (unrecognized "has an output and a return value")))
      (_
       (unrecognized "has a shape this test doesn't know"))))))

(define gir (find-gir "Everything" "1.0"))

(test-assert "Everything-1.0.gir was found" gir)

(when gir
  ;; The GIR describes the output of one_outparam_gchar, a gchar *, as
  ;; a string, so the C function stores one byte where the binding
  ;; expects a pointer.
  (for-each test-function (gir-callables gir))

  ;; How much of the library the tests above cover.
  (format #t "Everything: ~a functions in the GIR, ~a not bound, ~a not recognized~%"
          n-functions n-unbound n-unrecognized)
  (test-assert "the GIR lists functions" (> n-functions 100))
  (test-equal "every function in the GIR is bound" 0 n-unbound)
  (test-equal "every function in the GIR has a shape the test knows" 0 n-unrecognized))

(test-end "everything.scm")
