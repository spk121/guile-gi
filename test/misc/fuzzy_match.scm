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
   (send idx (insert "lorem" (new-data)))
   (send idx (insert "ipsum" (new-data)))
   (send idx (insert "dolor" (new-data)))
   (send idx (insert "sit" (new-data)))
   (send idx (insert "amet" (new-data)))
   (set! matches (send idx (match "lor" 0)))
   (display (map bv->match-data (vector->list matches)))
   (newline)))
