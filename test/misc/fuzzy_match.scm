(use-modules (gi)
             (srfi srfi-43)
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
 (let* ((idx (fuzzy-mutable-index:new #f))
        (matches
         (begin
           (for-each
            (lambda (str) (insert idx str (new-data)))
            '("lorem" "ipsum" "dolor" "sit" "amet"))
           (match idx "lor" 0))))
   (= 2
      ;; two matches
      (vector-length matches)
      ;; no #f
      (vector-count (lambda (i x) (and x (format #t "~a~%" (bv->match-data x))))
                    matches))))
