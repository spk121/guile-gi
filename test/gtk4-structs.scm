;;; Plain structs without a GType that GTK, GSK and GDK use, which
;;; guile-gi wraps as foreign objects.  None of these need a display.

(use-modules (gi) (gi repository) (gi types) (gi util)
             (oop goops)
             (rnrs bytevectors)
             (system foreign)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "gtk4-structs")

(define have-gtk4? (false-if-exception (require "Gtk" "4.0")))

(unless have-gtk4?
  (test-skip most-positive-fixnum))

(define (load-all specs)
  (for-each (lambda (spec) (load-by-name (car spec) (cadr spec))) specs))

(when have-gtk4?
  (load-all '(("Graphene" "Rect") ("Graphene" "Point") ("Graphene" "Size")
              ("Gdk" "RGBA")
              ("Gsk" "RoundedRect") ("Gsk" "ColorStop") ("Gsk" "Shadow")
              ("Gsk" "ParseLocation") ("Gsk" "RenderNode") ("Gsk" "ColorNode")
              ("Gsk" "LinearGradientNode") ("Gsk" "RoundedClipNode")
              ("Gsk" "ShadowNode")
              ("Gdk" "TimeCoord") ("Gdk" "ToplevelSize")
              ("Gtk" "PageRange") ("Gtk" "PrintSettings")
              ("Gtk" "CssLocation") ("Gtk" "CssSection")
              ("Gtk" "RecentData") ("GLib" "Bytes"))))

(define (rect x y w h)
  (let ((r (rect:alloc)))
    (rect:init r x y w h)
    r))

(define (point x y)
  (let ((p (point:alloc)))
    (point:init p x y)
    p))

(define (rgba str)
  (let ((c (make <GdkRGBA>)))
    (rgba:parse? c str)
    c))

;;; Constructors and accessors

(test-assert "class"
  (is-a? (make-rounded-rect) <GskRoundedRect>))

(test-equal "constructor with field values"
  '(1 3)
  (let ((r (make-page-range 1 3)))
    (list (page-range:start r) (page-range:end r))))

(test-equal "setter"
  7
  (let ((r (make-page-range)))
    (page-range:set-end! r 7)
    (page-range:end r)))

(test-equal "boxed field"
  "rgb(255,0,0)"
  (rgba:to-string (color-stop:color (make-color-stop 0.5 (rgba "red")))))

(test-equal "float fields"
  '(1.0 2.0 3.0)
  (let ((s (make-shadow (rgba "blue") 1.0 2.0 3.0)))
    (list (shadow:dx s) (shadow:dy s) (shadow:radius s))))

(test-equal "string, string list and boolean fields"
  '("Name" #f ("g1" "g2") #t ("x"))
  (let ((d (make-recent-data "Name" #f "text/plain" "app" "app %u" '("g1" "g2") #t)))
    (let ((before (list (recent-data:display-name d) (recent-data:description d)
                        (recent-data:groups d) (recent-data:is-private d))))
      (recent-data:set-groups! d #("x"))
      (append before (list (recent-data:groups d))))))

(test-equal "double array and flags fields"
  '(42 #f64(1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5))
  (let ((c (make-time-coord 42)))
    (time-coord:set-flags! c '())
    (time-coord:set-axes! c (make-f64vector 12 1.5))
    (list (time-coord:time c) (time-coord:axes c))))

(test-error "too many field values"
  #t
  (make-page-range 1 2 3))

(test-error "wrong type"
  'wrong-type-arg
  (page-range:start (make-css-location)))

(test-error "uninitialized"
  #t
  (page-range:start (make <GtkPageRange>)))

;;; Methods and arguments

(test-equal "methods"
  '(#f #t #f)
  (let ((rr (make-rounded-rect)))
    (rounded-rect:init-from-rect rr (rect 0.0 0.0 30.0 40.0) 5.0)
    (let ((inside (rounded-rect:contains-point? rr (point 15.0 20.0))))
      (rounded-rect:offset rr 100.0 0.0)
      (list (rounded-rect:is-rectilinear? rr)
            inside
            (rounded-rect:contains-point? rr (point 15.0 20.0))))))

(test-equal "struct argument and const struct return value"
  '(3 50)
  (let ((section (css-section:new #f (make-css-location 1 2 3 4 5)
                                  (make-css-location 10 20 30 40 50))))
    (list (css-location:lines (css-section:get-start-location section))
          (css-location:line-chars (css-section:get-end-location section)))))

(test-equal "a node keeps its own copy of a struct argument"
  30.0
  (let* ((rr (make-rounded-rect))
         (_ (rounded-rect:init-from-rect rr (rect 0.0 0.0 30.0 40.0) 0.0))
         (node (rounded-clip-node:new (color-node:new (rgba "red") (rect 0.0 0.0 1.0 1.0)) rr)))
    (rounded-rect:offset rr 100.0 0.0)
    (rect:get-width (rounded-rect:bounds (rounded-clip-node:get-clip node)))))

(test-equal "array argument and transfer-none array return value"
  '(0.0 0.5 1.0)
  (let ((node (linear-gradient-node:new (rect 0.0 0.0 10.0 10.0) (point 0.0 0.0) (point 10.0 0.0)
                                        (list (make-color-stop 0.0 (rgba "red"))
                                              (make-color-stop 0.5 (rgba "green"))
                                              (make-color-stop 1.0 (rgba "blue"))))))
    (map color-stop:offset (vector->list (linear-gradient-node:get-color-stops node)))))

(test-equal "struct array of another struct"
  4.0
  (let ((node (shadow-node:new (color-node:new (rgba "red") (rect 0.0 0.0 1.0 1.0))
                               (vector (make-shadow (rgba "black") 1.0 1.0 4.0)))))
    (shadow:radius (shadow-node:get-shadow node 0))))

(test-equal "transfer-full array return value"
  '((1 3) (7 9))
  (let ((settings (print-settings:new)))
    (print-settings:set-page-ranges settings (list (make-page-range 1 3) (make-page-range 7 9)))
    (map (lambda (r) (list (page-range:start r) (page-range:end r)))
         (vector->list (print-settings:get-page-ranges settings)))))

(test-assert "struct arguments to a callback"
  (let ((errors '()))
    (render-node:deserialize (bytes:new (string->utf8 "garbage {"))
                             (lambda (start end error data)
                               (set! errors (cons (list (parse-location:bytes start)
                                                        (parse-location:bytes end))
                                                  errors))))
    (and (pair? errors)
         (every (lambda (e) (<= (car e) (cadr e))) errors))))

(test-assert "survives collection"
  (let ((stops (map (lambda (i) (make-color-stop (/ i 100.0) (rgba "red"))) (iota 100))))
    (do ((i 0 (1+ i))) ((= i 1000))
      (make-recent-data "x" "y" "z" "a" "b" '("c") #f))
    (gc) (gc)
    (= 99 (inexact->exact (round (* 100 (color-stop:offset (list-ref stops 99))))))))

;;; Opaque structs

(test-equal "opaque struct from a NULL pointer"
  #f
  (pointer->toplevel-size %null-pointer))

(test-assert "opaque struct from a pointer"
  (is-a? (pointer->toplevel-size (make-pointer 1)) <GdkToplevelSize>))

(test-end "gtk4-structs")
