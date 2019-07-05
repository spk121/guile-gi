(use-modules (gi)
             (rnrs bytevectors)
             (system foreign)
             (test automake-test-lib))

(typelib-require ("GLib" "2.0") ("Dazzle" "1.0"))

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
 (let ((matches
        (with-object (fuzzy-mutable-index:new #f)
          (insert "lorem" (new-data))
          (insert "ipsum" (new-data))
          (insert "dolor" (new-data))
          (insert "sit" (new-data))
          (insert "amet" (new-data))
          (match "lor" 0))))
   (display (map bv->match-data (vector->list matches)))
   (newline)))
