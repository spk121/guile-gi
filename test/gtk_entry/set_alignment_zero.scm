(use-modules (gi) (gi repository)
             (test automake-test-lib))

(unless (false-if-exception (require "Gtk" "3.0"))
  (exit 'EXIT_SKIPPED))

(load-by-name "Gtk" "init")
(load-by-name "Gtk" "Entry")

;; This test exercises float32 argument conversions and return types.
(automake-test
 (begin
   (init)
   (let ([entry (entry:new)])
     (set-alignment entry 0)
     (format #t "entry: ~S~%" entry)
     (format #t "entry alignment: ~S~%" (get-alignment entry))
     (= 0 (get-alignment entry)))))
