(use-modules (gi) (gi repository)
             (test automake-test-lib))

(unless (false-if-exception (require "Gtk" "3.0"))
  (exit 'EXIT_SKIPPED))

(load-by-name "Gtk" "init_check")
(load-by-name "Gtk" "Entry")

;; This test exercises float32 argument conversions and return types.

(automake-test
 (if (not (init-check))
     'skipped
     (let ([entry1 (entry:new)]
           [entry2 (entry:new)])
       (set-alignment entry1 1.0)
       (set-alignment entry2 0.5)
       (format #t "entry 1: ~S~%" entry1)
       (format #t "entry 1 alignment: ~S~%" (get-alignment entry1))
       (format #t "entry 2: ~S~%" entry2)
       (format #t "entry 2 alignment: ~S~%" (get-alignment entry2))
       (and (= 1.0 (get-alignment entry1))
            (= 0.5 (get-alignment entry2))))))
