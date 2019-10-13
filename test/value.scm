(use-modules (gi) (srfi srfi-64))

(test-begin "value")

(test-equal "getset"
  42
  (let ((value (make <GValue>)))
    (set! (value G_TYPE_INT) 42)
    (value)))

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

(test-end "value")
