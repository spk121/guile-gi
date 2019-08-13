(use-modules (test automake-test-lib)
             (gi))

(automake-test
 (let* ((value (make <GValue>)))
   (set! (value G_TYPE_INT) 42)
   (= (value) 42)))
