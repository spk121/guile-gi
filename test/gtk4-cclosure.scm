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

(test-end "gtk4-cclosure")

