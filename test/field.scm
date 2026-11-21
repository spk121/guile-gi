;;; Field accessors of structs, unions and objects.

(use-modules (gi) (gi repository) (gi types) (gi util)
             (oop goops)
             (srfi srfi-64))

(test-begin "field")

(require "GLib" "2.0")
(require "GObject" "2.0")
(load-by-name "GLib" "PollFD")
(load-by-name "GLib" "Date")
(load-by-name "GObject" "Object")
(load-by-name "GObject" "Value")

(test-equal "getters and setters"
  '(7 3 0)
  (let ((fd (make <GPollFD>)))
    (poll-fd:set-fd! fd 7)
    (set! (poll-fd:events fd) 3)
    (list (poll-fd:fd fd) (poll-fd:events fd) (poll-fd:revents fd))))

(test-equal "GType field"
  G_TYPE_INT
  (let ((v (make <GValue>)))
    (set! (v G_TYPE_INT) 5)
    (value:g-type v)))

(test-assert "GType fields are read-only"
  (not (defined? 'value:set-g-type!)))

(test-equal "object fields"
  1
  (object:ref-count (make <GObject>)))

(test-assert "fields that guile-gi doesn't let Scheme change are read-only"
  (not (defined? 'object:set-ref-count!)))

(test-assert "no accessors for bitfields"
  ;; The typelib describes GDate's bitfields as whole fields past the
  ;; end of the struct.
  (not (defined? 'date:julian-days)))

(test-equal "wrong type"
  'wrong-type-arg
  (catch #t (lambda () (poll-fd:fd (make <GValue>)) 'no-error) (lambda (key . args) key)))

(test-error "wrong value type"
  #t
  (poll-fd:set-fd! (make <GPollFD>) "seven"))

;;; Embedded structs

(define graphene? (false-if-exception (require "Graphene" "1.0")))
(when graphene?
  (for-each (lambda (name) (load-by-name "Graphene" name)) '("Rect" "Point" "Size")))

(unless graphene?
  (test-skip 3))

(test-equal "embedded struct fields are copies"
  '(100.0 2.0 3.0 1.0)
  (let ((r (rect:alloc)))
    (rect:init r 1.0 2.0 3.0 4.0)
    (let ((origin (rect:origin r)))
      (set! (point:x origin) 100.0)
      (list (point:x origin) (point:y origin) (size:width (rect:size r))
            (point:x (rect:origin r))))))

(test-equal "embedded struct setter"
  '(5.0 6.0)
  (let ((r (rect:alloc))
        (p (point:alloc)))
    (rect:init r 1.0 2.0 3.0 4.0)
    (point:init p 5.0 6.0)
    (set! (rect:origin r) p)
    (list (rect:get-x r) (rect:get-y r))))

(test-equal "embedded struct setter type check"
  'wrong-type-arg
  (catch #t
    (lambda () (set! (rect:origin (rect:alloc)) (rect:alloc)) 'no-error)
    (lambda (key . args) key)))

(test-end "field")
