(use-modules (gi)
             (gi types)
             (srfi srfi-64))

(test-begin "type")

(test-equal "G_TYPE_LONG name"
  "glong"
  (gtype-get-name G_TYPE_LONG))

(define <Alpha> #f)

(test-assert "Define new <Alpha> class"
  (integer?
   (begin
     (set! <Alpha> (register-type "Alpha" <GObject> '() '()))
     (get-gtype <Alpha>))))

(test-equal "<Alpha> class roundtrip"
  <Alpha>
  (gtype-get-scheme-type (get-gtype <Alpha>)))

(test-equal "<Alpha> class has <GObject> as parent"
  G_TYPE_OBJECT
  (gtype-get-parent (get-gtype <Alpha>)))

(test-assert "<Alpha> class is a <GObject>"
  (gtype-is-a? (get-gtype <Alpha>) (get-gtype <GObject>)))

(test-assert "<GObject> has <Alpha> as child"
  (member
   (get-gtype <Alpha>)
   (gtype-get-children G_TYPE_OBJECT)))

(test-equal "<Alpha> other type properties"
  `(,G_TYPE_OBJECT () () 2 #f #t #t #t)
  (let ((type (get-gtype <Alpha>)))
    (list
     (gtype-get-fundamental type)
     (gtype-get-children type)
     (gtype-get-interfaces type)
     (gtype-get-depth type)
     (gtype-is-interface? type)
     (gtype-is-classed? type)
     (gtype-is-instantiatable? type)
     (gtype-is-derivable? type)
     )))

(test-assert "%gtype-dump-table returns a list"
  (list? (%gtype-dump-table)))

(test-end "type")
