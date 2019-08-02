(define-module (gi core-generics)
  #:use-module (oop goops))

(define-syntax-rule (ensure-generic! x module)
  (begin
    (define x (ensure-generic (@ module x) 'x))
    (export! x)))

(ensure-generic! connect (guile))
(ensure-generic! command-line (guile))
(ensure-generic! equal? (guile))
(ensure-generic! format (guile))
(ensure-generic! load (guile))
(ensure-generic! send (guile))
(ensure-generic! shutdown (guile))
(ensure-generic! quit (guile))
(ensure-generic! write (guile))
