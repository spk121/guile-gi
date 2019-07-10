(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("GObject" "2.0"))

(define sum
  (make-signal
   #:name "sum"
   #:return-type G_TYPE_INT
   #:accumulator +))

(define <TestParam>
  (register-type
   "TestParam"
   <GObject>
   '()
   (list sum)))

(automake-test
 (let ((object (make-gobject <TestParam>)))
   (connect object sum (const 1))
   (connect object sum (const 2))
   (connect object sum (const 3))
   (= (sum object) 6)))
