(use-modules (gi) (gi types) (gi repository)
             (srfi srfi-64))

(test-begin "insanity.scm")

(require "GObject" "2.0")
(require "GLib" "2.0")
(let ((m (make-module)))
  (save-module-excursion
   (lambda ()
     (set-current-module m)
     (load-by-name "GObject" "Object")
     (load-by-name "GObject" "Value")
     (load-by-name "GObject" "Closure")))

  (test-equal "GObject.Object"
    <GObject>
    (module-ref m '<GObject>))

  (test-equal "GObject.Value"
    <GValue>
    (module-ref m '<GValue>))

  (test-equal "GObject.Closure"
    <GClosure>
    (module-ref m '<GClosure>)))

(test-end "insanity.scm")
