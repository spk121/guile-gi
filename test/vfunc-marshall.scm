;;; Overriding virtual functions with the arguments GIMarshallingTests
;;; exercises: its C methods call the virtual functions.

(use-modules (gi) (gi types) (gi util)
             (oop goops)
             (srfi srfi-64))

(test-begin "vfunc-marshall")

(use-typelibs (("Marshall" "1.0") #:renamer (protect* '(sizeof short int long size_t)))
              (("GLib" "2.0") #:renamer (protect* '(test-equal test-assert test-skip int))))

(define received #f)

(define <Sub>
  (register-type
   "VfuncMarshallSub" <MarshallObject>
   #:vfuncs
   `((vfunc-return-value-only . ,(lambda (self) 42))
     (vfunc-one-out-parameter . ,(lambda (self) 4.5))
     (vfunc-multiple-out-parameters . ,(lambda (self) (values 1.5 2.5)))
     (vfunc-return-value-and-one-out-parameter . ,(lambda (self) (values 7 8)))
     (vfunc-return-value-and-multiple-out-parameters . ,(lambda (self) (values 7 8 9)))
     (vfunc-one-inout-parameter . ,(lambda (self a) (* a 2)))
     (vfunc-multiple-inout-parameters . ,(lambda (self a b) (values (* a 2) (* b 3))))
     (vfunc-return-value-and-one-inout-parameter . ,(lambda (self a) (values 5 (* a 2))))
     (vfunc-array-out-parameter . ,(lambda (self) #f32(1.0 2.0 3.0)))
     (vfunc-return-enum . ,(lambda (self) (symbol->enum 'value2)))
     (vfunc-out-flags . ,(lambda (self) (list->flags '(value2))))
     (vfunc-return-object-transfer-full . ,(lambda (self) (make <GObject>)))
     (vfunc-return-object-transfer-none
      . ,(lambda (self) (set! received (make <GObject>)) received))
     (vfunc-in-object-transfer-full . ,(lambda (self obj) (set! received obj)))
     (vfunc-meth-with-err . ,(lambda (self x) (error "failed with" x)))
     ;; The callback's closure data is passed along, as for callbacks.
     (vfunc-with-callback . ,(lambda (self callback data) (set! received (callback 41 data))))
     (method-int8-in . ,(lambda (self in) (set! received in))))))

(define o (make <Sub>))

(define-syntax-rule (values->list expr)
  (call-with-values (lambda () expr) list))

(test-equal "return value" 42 (object:vfunc-return-value-only o))
(test-equal "out parameter" 4.5 (object:vfunc-one-out-parameter o))
(test-equal "out parameters" '(1.5 2.5) (values->list (object:vfunc-multiple-out-parameters o)))
(test-equal "return value and out parameter"
  '(7 8) (values->list (object:vfunc-return-value-and-one-out-parameter o)))
(test-equal "return value and out parameters"
  '(7 8 9) (values->list (object:vfunc-return-value-and-multiple-out-parameters o)))
(test-equal "inout parameter" 3.0 (object:vfunc-one-inout-parameter o 1.5))
(test-equal "inout parameters"
  '(2.0 6.0) (values->list (object:vfunc-multiple-inout-parameters o 1.0 2.0)))
(test-equal "return value and inout parameter"
  '(5 6) (values->list (object:vfunc-return-value-and-one-inout-parameter o 3)))
(test-equal "array out parameter" #f32(1.0 2.0 3.0) (object:vfunc-array-out-parameter o))
(test-equal "enum return value" 'value2 (enum->symbol (object:vfunc-return-enum o)))
(test-equal "flags out parameter" '(value2) (flags->list (object:vfunc-out-flags o)))

(test-equal "in parameter"
  7
  (begin (object:method-int8-in o 7) received))

(test-assert "object return values"
  (and (is-a? (object:do-vfunc-return-object-transfer-full <Sub> o) <GObject>)
       (is-a? (object:do-vfunc-return-object-transfer-none <Sub> o) <GObject>)))

(test-assert "transfer-full object argument"
  (begin
    (object:do-vfunc-in-object-transfer-full <Sub> o (make <GObject>))
    (gc) (gc)
    (is-a? received <GObject>)))

(test-equal "callback argument"
  41
  (begin (object:call-vfunc-with-callback o) received))

(test-equal "Scheme errors become GErrors"
  'misc-error
  (catch #t (lambda () (object:vfunc-meth-with-error? o 1) 'no-error) (lambda (key . args) key)))

(test-equal "invoker of the parent's implementation"
  5
  (begin
    (object:do-method-with-default-implementation <MarshallObject> o 5)
    (object:int o)))

(test-end "vfunc-marshall")
