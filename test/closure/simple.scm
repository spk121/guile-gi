(use-modules (test automake-test-lib)
             (gi))

(define (int->value i)
  (let ((v (make <GValue>)))
    (set! (v G_TYPE_INT) i)
    v))

(automake-test
 (= ((apply (procedure->closure *) G_TYPE_INT (map int->value '(21 2)))) 42))
