(use-modules (gi) (gi repository) (gi util)
             (oop goops)
             (srfi srfi-64))

(test-begin "oop")

(use-typelibs (("GObject" "2.0") #:renamer (protect register-type)))

(define test-param #f)
(define test-signal #f)
(define test-detailed-signal #f)
(define <TestClass> #f)

(test-assert "register-type"
  (begin
    (set! test-signal
          (make-signal
           #:name "test-signal"
           #:return-type G_TYPE_NONE))

    (set! test-detailed-signal
          (make-signal
           #:name "test-detailed-signal"
           #:flags (list->signal-flags '(detailed))
           #:return-type G_TYPE_NONE))

    (set! test-param
          (param-spec-int
           "test-param"
           "test-param"
           "This is a test parameter"
           -200 200 0
           (list->param-flags '(readwrite))))

    (set! <TestClass>
          (register-type
           "TestClass"
           <GObject>
           (list test-param)
           (list test-signal test-detailed-signal)))))

(unless (test-passed?)
  (test-skip most-positive-fixnum))

(define object #f)

(test-assert "create object"
  (set! object (make <TestClass>)))

(unless (test-passed?)
  (test-skip most-positive-fixnum))

(test-equal "set property"
  200
  (begin
    (set! (test-param object) 200)
    (test-param object)))

;; FIXME: value remains unchanged with a warning logged
;;        maybe we should throw instead?
(test-equal "set invalid value"
  200
  (begin
    (set! (test-param object) -400)
    (test-param object)))

(test-equal "bind-property"
  -100
  (let ((object2 (make <TestClass>))
        (transformer (procedure->closure
                      (lambda (binding src dst) (values #t (- src)))
                      #*001)))
    (bind-property-full object "test-param"
                        object2 "test-param"
                        (list->binding-flags '(default))
                        transformer
                        transformer)
    (set! (test-param object) 100)
    (test-param object2)))

(test-assert "simple signal"
  (let ((success #f))
    (connect object test-signal (lambda _ (set! success #t)))
    (test-signal object)
    success))

(let ((detail-fired #f)
      (wrong-detail-fired #f))
  (test-assert "connect detailed"
    (begin
      (connect object test-detailed-signal 'detail
               (lambda _ (set! detail-fired #t)))
      (connect object test-detailed-signal 'wrong-detail
               (lambda _ (set! wrong-detail-fired #t)))

      (test-detailed-signal object 'detail)))

  (unless (test-passed?)
    (test-skip 2))

  (test-equal "detail fired"
    #t detail-fired)

  (test-equal "wrong detail not fired"
    #f wrong-detail-fired))

(test-assert "blocked signal"
  (let ((blocked #t))
    (signal-handler-block
     object
     (connect object test-signal (lambda _ (set! blocked #f))))
    (test-signal object)
    blocked))

(if (false-if-exception (require "Gio" "2.0"))
    (begin
      (test-assert "interface"
        (begin
          ;; Test that Applications are ActionMaps, which they should be
          (load-by-name "Gio" "ActionMap")
          (load-by-name "Gio" "Application")
          (memq <GActionMap> (class-precedence-list <GApplication>)))))
    (begin
      (test-skip "interface")
      (test-assert "interface" #f)))

(test-end "oop")
