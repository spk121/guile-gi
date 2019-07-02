(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (system foreign)
             (test automake-test-lib))

(catch #t
  (lambda () (typelib-load "Dazzle" "1.0"))
  (lambda args (exit EXIT_SKIPPED)))

(define new-data
  (let ((idx -1))
    (lambda ()
      (set! idx (1+ idx))
      (make-pointer idx))))

(define (bv->match-data bv)
  (let ((c-struct
         (parse-c-struct
          (bytevector->pointer bv)
          (list '* '* float int))))
    (cons
     (pointer->string (car c-struct))
     (cdr c-struct))))

(automake-test
 (let ((idx (fuzzy-mutable-index:new #f))
       (matches '()))
   (with-object idx
     (insert "lorem" (new-data))
     (insert "ipsum" (new-data))
     (insert "dolor" (new-data))
     (insert "sit" (new-data))
     (insert "amet" (new-data)))
   (set! matches (with-object idx (match "lor" 0)))
   (display (map bv->match-data (vector->list matches)))
   (newline)))
