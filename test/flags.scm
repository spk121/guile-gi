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

(define-class <ZeroFlags> (<GFlags>))
(class-slot-set! <ZeroFlags> 'obarray
                 (alist->hashq-table
                  '((none . 0)
                    (a . 1)
                    (b . 2))))

(define-class <Enum> (<GEnum>))
(class-slot-set! <Enum> 'obarray
                 (alist->hashq-table
                  '((x . 0)
                    (y . 1))))

(test-begin "flags.scm")

(test-equal "difference of overlapping sets"
  ;; Used to be the symmetric difference, (a b).
  (list->flags <Flags> '(a))
  (flags-difference (list->flags <Flags> '(a c))
                    (list->flags <Flags> '(b c))))

(test-equal "flags->list omits zero-valued flags when something is set"
  '(a)
  (flags->list (list->flags <ZeroFlags> '(a))))

(test-equal "flags->list of no flags lists zero-valued flags"
  '(none)
  (flags->list (number->flags <ZeroFlags> 0)))

(test-equal "number->enum reports the number"
  '(99)
  (catch 'out-of-range
    (lambda () (number->enum <Enum> 99))
    (lambda (key subr message args data) data)))

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
