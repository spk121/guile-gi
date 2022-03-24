(use-modules (gi) (gi types) (gi repository)
             (srfi srfi-64))

(test-begin "il.scm")

(test-equal "'require' returns IL for ^library - func"
  '^library
  (let ((il (require "GObject" "2.0")))
    (car il)))

(define $il #f)
(test-equal "load-by-name returns IL for ^constant - func"
  '^constant
  (call-with-values
      (lambda () (load-by-name "GObject" "PARAM_MASK"))
    (lambda (symbols il)
      (set! $il il)
      (car il))))

(test-equal "load-by-name returns IL for ^constant - name"
  'PARAM_MASK
  (cadr $il))

(test-end "il.scm")
