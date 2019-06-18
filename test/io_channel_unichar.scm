(use-modules (gi) (gi glib-2)
             (rnrs bytevectors)
             (srfi srfi-1)
             (test automake-test-lib))

(define SIZ 10)

(define (subbytevector bv start end)
  (let ((bv2 (make-bytevector (- end start))))
    (bytevector-copy! bv start bv2 0 (- end start))
    bv2))

(automake-test
 (begin
   ;; Let's make a channel that writes to a file.
   (let ((channel (IOChannel-new-file "tmp.txt" "w")))
     (send channel (write-unichar #\α))
     (send channel (write-unichar #\β))
     (send channel (shutdown #t)))

   ;; Now, make a channel that reads from that file.
   (let* ((channel (IOChannel-new-file "tmp.txt" "r"))
          (alpha (second (send channel (read-unichar))))
          (beta  (second (send channel (read-unichar)))))
     (write alpha) (newline)
     (write beta) (newline)
     (and
      (equal? alpha #\α)
      (equal? beta #\β)))))
