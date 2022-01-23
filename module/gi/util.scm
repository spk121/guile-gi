(define-module (gi util)
  #:use-module (gi core util)
  #:re-export(push-duplicate-handler!
              protect protect*
              %rnrs-syntax
              %r5rs-procedures
              short-vector->list
              int-vector->list
              long-vector->list
              list->long-vector
              list->int-vector
              list->short-vector))
