(define-module (gi util)
  #:use-module (ice-9 optargs)
  #:export (push-duplicate-handler!
            protect protect* %rnrs-syntax))

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
