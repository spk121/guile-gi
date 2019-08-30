(use-modules (gi) (gi repository)
             (srfi srfi-26)
             (rnrs bytevectors)
             (test automake-test-lib))

(unless (false-if-exception (require "Gtk" "3.0"))
  (exit 'EXIT_SKIPPED))

(for-each (cute load-by-name "Gtk" <>) '("init_check" "Box" "Orientation"))

(automake-test
 (if (not (init-check))
     'skipped
     (let* ([box1 (box:new (symbol->orientation 'vertical) 1)]
            [box2 (make <GtkBox>
                    #:orientation (symbol->orientation 'vertical)
                    #:spacing 2)])
       (format #t "box 1: ~S~%" box1)
       (format #t "box 1 spacing: ~S~%" (spacing box1))
       (format #t "box 2: ~S~%" box2)
       (format #t "box 2 spacing: ~S~%" (spacing box2))
       (and (is-a? box1 <GtkBox>)
            (= 1 (spacing box1))
            (is-a? box2 <GtkBox>)
            (= 2 (spacing box2))))))
