(use-modules (gi) (gi types) (gi repository)
             (srfi srfi-64))

(test-begin "il.scm")

(test-equal "'require' returns IL for ^library"
  '^library
  (let ((il (require "GObject" "2.0")))
    (car il)))

(test-end "il.scm")
