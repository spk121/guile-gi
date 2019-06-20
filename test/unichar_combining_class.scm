(use-modules (gi) (gi glib-2)
             (test automake-test-lib))

(automake-test
 (begin 
   (format #t "The combining class of ~s is ~s~%"
           #\と (unichar-combining-class #\と))
   (format #t "The combining class of ~s is ~s~%"
           #\◌̀ (unichar-combining-class #\◌̀))
   (and (zero? (unichar-combining-class #\と)) ; Not a combining glyph
        (equal? 230 (unichar-combining-class #\◌̀))))) ; Combines from above
