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

(test-error "connect unknown signal"
  #t
  (connect object
           (make-signal #:name "no-such-signal" #:return-type G_TYPE_NONE)
           (lambda _ #t)))

;; A user-defined type derived from another user-defined type, each
;; with a property of its own.
(define sub-param
  (param-spec-string "sub-param" "sub-param" "A string parameter" "default"
                     (list->param-flags '(readwrite))))

(define <TestSubClass>
  (register-type "TestSubClass" <TestClass> (list sub-param) '()))

(test-equal "subclass property defaults"
  '(0 "default")
  (let ((sub (make <TestSubClass>)))
    (list (test-param sub) (sub-param sub))))

(test-equal "subclass properties are independent"
  '(150 "changed")
  (let ((sub (make <TestSubClass>)))
    (set! (sub-param sub) "changed")
    (set! (test-param sub) 150)
    (list (test-param sub) (sub-param sub))))

(test-assert "subclass instances can be collected"
  (begin
    (do ((i 0 (1+ i))) ((= i 20)) (make <TestSubClass>))
    (do ((i 0 (1+ i))) ((= i 5)) (gc))
    #t))

(if (false-if-exception (require "Gio" "2.0"))
    (begin
      (load-by-name "Gio" "ListModel")
      (load-by-name "Gio" "ListStore")
      (load-by-name "Gio" "SimpleAction")

      (let ((store (list-store:new (get-gtype <GSimpleAction>)))
            (action (simple-action:new "one" #f))
            (other (simple-action:new "two" #f)))
        (list-store:append store action)
        (list-store:append store other)
        (test-assert "wrappers of the same object are equal?"
          (let ((a (list-model:get-item store 0))
                (b (list-model:get-item store 0)))
            (and (equal? a b) (equal? a action))))
        (test-assert "wrappers of different objects are not equal?"
          (not (equal? (list-model:get-item store 0)
                       (list-model:get-item store 1)))))

      (test-assert "interface"
        (begin
          ;; Test that Applications are ActionMaps, which they should be
          (load-by-name "Gio" "ActionMap")
          (load-by-name "Gio" "Application")
          (memq <GActionMap> (class-precedence-list <GApplication>)))))
    (begin
      (test-skip 3)
      (test-assert "interface" #f)))

(test-end "oop")
