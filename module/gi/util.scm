(define-module (gi util)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:export (push-duplicate-handler!
            protect protect*
            %rnrs-syntax
            %r5rs-procedures
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

(define %r5rs-procedures
  (cdr
   '(<>
     eqv? eq? equal?
     number? complex? real? rational? integer?
     exact? inexact?
     zero? positive? negative? odd? even?
     max min
     + * - /
     abs
     quotient remainder modulo
     gcd lcm
     numerator denominator
     rationalize
     floor ceiling truncate round
     exp log sin cos tan asin acos atan
     sqrt
     expt
     make-rectangular make-polar real-part imag-part magnitude angle
     exact->inexact inexact->exact
     number->string string->number
     boolean?
     not
     pair?
     cons car cdr
     set-car! set-cdr!
     caar cadr cdar cddr
     caaar caadr cadar caddr cdaar cdadr cddar cdddr
     caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
     cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
     null?
     list?
     list
     length
     append
     reverse
     list-tail list-ref
     memq memv member
     assq assv assoc
     symbol?
     symbol->string string->symbol
     char?
     char=? char<? char>? char<=? char>=?
     char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
     char-alphabetic? char-numeric? char-whitespace?
     char-upper-case? char-lower-case?
     char->integer integer->char
     char-upcase
     char-downcase
     string?
     make-string
     string
     string-length
     string-ref string-set!
     string=? string-ci=?
     string<? string>? string<=? string>=?
     string-ci<? string-ci>? string-ci<=? string-ci>=?
     substring
     string-length
     string-append
     string->list list->string
     string-copy string-fill!
     vector?
     make-vector
     vector
     vector-length
     vector-ref vector-set!
     vector->list list->vector
     vector-fill!
     procedure?
     apply
     map
     for-each
     force
     call-with-current-continuation
     values
     call-with-values
     dynamic-wind
     eval
     input-port? output-port?
     current-input-port current-output-port
     read
     read-char
     peek-char
     eof-object?
     char-ready?
     write
     display
     newline
     write-char)))

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

