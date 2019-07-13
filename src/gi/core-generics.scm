(define-module (gi core-generics)
  #:use-module (oop goops))

(define-syntax-rule (ensure-generic! x module)
  (begin
    (define x (ensure-generic (@ module x) 'x))
    (export! x)))

(ensure-generic! connect (guile))
(ensure-generic! equal? (guile))
(ensure-generic! format (guile))
(ensure-generic! send (guile))
(ensure-generic! quit (guile))
(ensure-generic! write (guile))
