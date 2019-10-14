(define-module (gi util)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:export (push-duplicate-handler!
            protect protect* %rnrs-syntax
            short-vector->list
            int-vector->list
            long-vector->list
            list->long-vector
            list->int-vector
            list->short-vector
            ))

(define (shrug-equals module name int1 val1 int2 val2 var val)
  (and (eq? val1 val2)
       (make-variable val1)))

(module-define! duplicate-handlers 'shrug-equals shrug-equals)

(define (push-duplicate-handler! handler)
  (default-duplicate-binding-handler
   (cons handler
         (default-duplicate-binding-handler))))

(define %rnrs-syntax
  (cdr
   '(<>
     begin define lambda
     let let* let-values let*-values letrec letrec*
     quote quasiquote unquote unquote-splicing
     syntax define-syntax let-syntax letrec-syntax
     syntax-rules syntax-case
     with-syntax quasisyntax unsyntax unsyntax-splicing syntax-violation)))

(define epsilon '#{}#)
(define (symbol-empty? symbol) (equal? symbol epsilon))

(define* (protect symbol #:optional (prefix epsilon) (suffix epsilon))
  (when (and (symbol-empty? prefix)
             (symbol-empty? suffix))
    (set! prefix '%))

  (lambda (sym)
    (if (eq? sym symbol)
        (symbol-append prefix sym suffix)
        sym)))

(define* (protect* syms #:optional (prefix epsilon) (suffix epsilon))
  (when (and (symbol-empty? prefix)
             (symbol-empty? suffix))
    (set! prefix '%))
  (lambda (sym)
    (if (memq sym syms)
        (symbol-append prefix sym suffix)
        sym)))

(define (short-vector->list x)
  (cond
   ((= (sizeof short) 2)
    (s16vector->list x))
   ((= (sizeof short) 4)
    (s32vector->list x))
   (else
    (error "unknown short size"))))

(define (list->short-vector x)
  (cond
   ((= (sizeof short) 2)
    (list->s16vector x))
   ((= (sizeof short) 4)
    (list->s32vector x))
   (else
    (error "unknown short size"))))

(define (int-vector->list x)
  (cond
   ((= (sizeof int) 4)
    (s32vector->list x))
   ((= (sizeof int) 8)
    (s64vector->list x))
   (else
    (error "unknown int size"))))

(define (list->int-vector x)
  (cond
   ((= (sizeof int) 4)
    (list->s32vector x))
   ((= (sizeof int) 8)
    (list->s64vector x))
   (else
    (error "unknown int size"))))

(define (long-vector->list x)
  (cond
   ((= (sizeof long) 4)
    (s32vector->list x))
   ((= (sizeof long) 8)
    (s64vector->list x))
   (else
    (error "unknown long int size"))))

(define (list->long-vector x)
  (cond
   ((= (sizeof long) 4)
    (list->s32vector x))
   ((= (sizeof long) 8)
    (list->s64vector x))
   (else
    (error "unknown long int size"))))
