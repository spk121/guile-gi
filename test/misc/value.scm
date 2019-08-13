(use-modules (test automake-test-lib)
             (gi))

(automake-test
 (let* ((value (make <GValue>)))
   (and
    (begin
      (set! (value G_TYPE_INT) 42)
      (= (value) 42))
    (begin
      (set! (value G_TYPE_UCHAR) 255)
      (= (transform value G_TYPE_CHAR) -1)))))
