(use-modules (gi) (gi types) (gi repository)
             (srfi srfi-64))

(test-begin "il.scm")

(define (gather func)
  (let ((port (open-output-string)))
    (set-il-output-port port)
    (func)
    (let ((ret (get-output-string port)))
      (close-output-port port)
      ret)))

(define test1str "(^library \"GObject\"")
(test-equal "'require' returns IL for ^library - func"
  test1str
  (string-take (gather (lambda() (require "GObject" "2.0")))
               (string-length test1str)))

#|
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

(test-equal "load-by-name returns IL for ^type - func"
  '^type
  (call-with-values
      (lambda () (load-by-name "GObject" "GObject"))
    (lambda (symbols il)
      (set! $il il)
      (car il))))

(test-equal "load-by-name returns IL for ^type - name"
  '<GObject>
  (cadr $il))
|#

(test-end "il.scm")
