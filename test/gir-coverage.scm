;;; How much of the test libraries guile-gi binds.
;;;
;;; For each library this compares the functions, methods and
;;; constructors in its GIR file with the procedures that loading its
;;; typelib defines.  Whatever is missing has to be listed below with
;;; the reason, so that a function guile-gi stops binding fails the test,
;;; and so does a function on the list that it starts to bind: that one
;;; needs taking off the list, and a test of its own.

(use-modules (gi) (gi repository)
             (test gir)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "gir-coverage")

;; MarshallSimpleStruct and MarshallPointerStruct have no GType.
;; guile-gi binds such structs only if it has been taught their layout,
;; which it has for the ones GTK needs; functions that take or return
;; any other are left out.
(define structs-without-gtype
  '("array-simple-struct-in"
    "array-fixed-out-struct"
    "array-fixed-out-struct-uninitialized"
    "array-fixed-caller-allocated-struct-out"
    "simple-struct:returnv"
    "simple-struct:inv"
    "simple-struct:method"
    "pointer-struct:returnv"
    "pointer-struct:inv"))

(define libraries
  ;; name, version, the least number of callables its GIR should have,
  ;; and what is known not to be bound
  `(("Everything" "1.0" 100 ())
    ("Marshall" "1.0" 400 ,structs-without-gtype)
    ("Extra" "1.0" 20 ())))

(define (test-library name version at-least known-unbound)
  (define gir (find-gir name version))
  (test-assert (string-append name ": the GIR was found") gir)
  (when gir
    (let* ((module (typelib->module (list 'test 'gir-coverage (string->symbol name))
                                    name version))
           (callables (gir-callables gir))
           (names (map callable-scheme-name callables))
           (unbound (map callable-scheme-name
                         (remove (lambda (callable) (find-binding module callable))
                                 callables))))
      (format #t "~a: ~a functions, methods and constructors in the GIR, ~a bound (~a%)~%"
              name (length callables) (- (length callables) (length unbound))
              (round (* 100. (/ (- (length callables) (length unbound))
                                (max 1 (length callables))))))
      (test-assert (string-append name ": the GIR lists its functions")
        (>= (length callables) at-least))
      (test-equal (string-append name ": everything else is bound")
        '()
        (lset-difference string=? unbound known-unbound))
      (test-equal (string-append name ": what is listed as not bound is not bound")
        '()
        (lset-difference string=? known-unbound unbound))
      (test-equal (string-append name ": what is listed as not bound is in the GIR")
        '()
        (lset-difference string=? known-unbound names)))))

(for-each (lambda (library) (apply test-library library)) libraries)

(test-end "gir-coverage")
