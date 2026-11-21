(use-modules (gi) (gi repository) (gi types) (gi util)
             (oop goops)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "gtk4-cclosure")

(define load-by-name?
  (compose
   (negate null?)
   load-by-name))

(unless (false-if-exception (require "Gtk" "4.0"))
  (test-skip most-positive-fixnum))

(test-assert "<GtkExpression> exists"
  (load-by-name? "Gtk" "Expression"))

(test-assert "<GtkCClosureExpression> exists"
  (load-by-name? "Gtk" "CClosureExpression"))

;;; GtkExpression is a fundamental instance type that GTK introduces,
;;; with its own ref, unref and GValue functions.  Values of it used to
;;; abort with "unhandled argument type" or raise "unknown type".

(for-each (lambda (name) (load-by-name "Gtk" name))
          '("Expression" "ConstantExpression" "PropertyExpression"
            "StringFilter" "StringObject"))

(define (string-value str)
  (let ((v (make <GValue>)))
    (set! (v G_TYPE_STRING) str)
    v))

(test-assert "fundamental instance return value"
  (is-a? (constant-expression:new-for-value (string-value "abc"))
         <GtkConstantExpression>))

(test-assert "fundamental instance class precedence"
  (let ((cpl (class-precedence-list <GtkConstantExpression>)))
    (and (memq <GtkExpression> cpl)
         (memq (@@ (gi oop) <GFundamental>) cpl))))

(test-equal "fundamental instance argument"
  '(#t "abc")
  (let ((expr (constant-expression:new-for-value (string-value "abc")))
        (v (make <GValue>)))
    (list (expression:evaluate? expr #f v) (v))))

(test-assert "fundamental instance transfer-full argument and return value"
  (let ((filter (string-filter:new
                 (property-expression:new (get-gtype <GtkStringObject>) #f "string"))))
    (is-a? (string-filter:get-expression filter) <GtkPropertyExpression>)))

(test-error "fundamental instance argument type check"
  'wrong-type-arg
  (string-filter:set-expression (string-filter:new #f) 42))

(test-equal "fundamental instance properties"
  '(#f "abc")
  (let ((filter (string-filter:new #f))
        (expr (constant-expression:new-for-value (string-value "abc"))))
    (let ((before (expression filter)))
      (set! (expression filter) expr)
      (let ((v (make <GValue>)))
        (expression:evaluate? (expression filter) #f v)
        (list before (v))))))

(test-assert "fundamental instances survive collection"
  (let ((filter (string-filter:new #f)))
    (set! (expression filter)
          (constant-expression:new-for-value (string-value "abc")))
    (do ((i 0 (1+ i))) ((= i 1000))
      (constant-expression:new-for-value (string-value "x")))
    (gc) (gc)
    (let ((v (make <GValue>)))
      (and (expression:evaluate? (expression filter) #f v)
           (equal? (v) "abc")))))

(test-end "gtk4-cclosure")

