(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (srfi srfi-1)
             (test automake-test-lib))

(define SIZ 10)

(define (subbytevector bv start end)
  (let ((bv2 (make-bytevector (- end start))))
    (bytevector-copy! bv start bv2 0 (- end start))
    bv2))

(define-syntax-rule (second-value body)
  (call-with-values (lambda () body)
    (lambda args (second args))))

(automake-test
 (begin
   ;; Let's make a channel that writes to a file.
   (let ((channel (iochannel:new-file "tmp.txt" "w")))
     (with-object channel
       (write-unichar #\α)
       (write-unichar #\β)
       (shutdown #t)))

   ;; Now, make a channel that reads from that file.
   (let* ((channel (iochannel:new-file "tmp.txt" "r"))
          (alpha (second-value (with-object channel (read-unichar))))
          (beta  (second-value (with-object channel (read-unichar)))))
     (write alpha) (newline)
     (write beta) (newline)
     (and
      (equal? alpha #\α)
      (equal? beta #\β)))))
