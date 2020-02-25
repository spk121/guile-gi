(use-modules (gi types)
             (oop goops)
             (ice-9 hash-table)

             (srfi srfi-64))

(define-class <Flags> (<GFlags>))
(define-class <OtherFlags> (<GFlags>))

(class-slot-set! <Flags> 'obarray
                 (alist->hashq-table
                  '((a . 1)
                    (b . 2)
                    (ab . 3)
                    (c . 4)
                    (d . 8))))

(class-slot-set! <OtherFlags> 'obarray
                 (alist->hashq-table
                  '((a . 16)
                    (b . 8)
                    (c . 2)
                    (d . 1)
                    (cd . 3))))

(test-begin "flags.scm")

(test-equal "complement"
  (list->flags <Flags> '(c d))
  (flags-complement (list->flags <Flags> '(ab))))

(test-equal "difference"
  (list->flags <Flags> '(a))
  (flags-difference (list->flags <Flags> '(ab))
                    (list->flags <Flags> '(b))))

(test-assert "union"
  (flags-set?
   (flags-union (list->flags <Flags> '(a))
                (list->flags <Flags> '(b)))
   '(a b ab)))

(let ((f1 (list->flags <Flags> '(ab)))
      (f2 (list->flags <OtherFlags> '(a b))))
  (test-assert "numerical-projection"
    (= (flags-projection/number f1 f2) f1))
  (test-assert "numerical-projection-outside-mask"
    (not (= (flags-projection/number f2 f1) f1)))
  (test-assert "projection-breaks-equal"
    (not (equal? (flags-projection/number f1 f2) f1)))
  (test-assert "list-projection-preserves-flags"
    (and
     (flags-set? (flags-projection/list f1 f2) '(a b))
     (flags-set? (flags-projection/list f2 f1) '(ab))
     (flags-set? (flags-projection/list
                  (list->flags <OtherFlags> '(cd))
                  <Flags>)
                 '(c d)))))

(test-end "flags.scm")
