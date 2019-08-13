(use-modules (test automake-test-lib)
             (gi)
             (gi value))

(init-value-type)

(automake-test
 (let* ((value (make <GValue>))
        (accessor (value->accessor value)))
   (and
    (begin
      (set! (accessor G_TYPE_INT) 42)
      (= (accessor) 42))
    (begin
      (set! (accessor G_TYPE_UCHAR) 255)
      (= (transform value G_TYPE_CHAR) -1)))))
