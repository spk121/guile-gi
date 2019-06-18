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
     (let ((result (send channel (write-chars (string->utf8 "foobar") 6))))
       (send channel (shutdown #t))))

   ;; Now, make a channel that reads from that file.
   (let ((channel (IOChannel-new-file "tmp.txt" "r"))
         (buf (make-bytevector SIZ 0)))

     ;; Read as much as we can.
     (let ((status/nbytes (send channel (read-chars buf SIZ))))
       (let ((status      (first  status/nbytes))
             (nbytes      (second status/nbytes)))
         (let ((bv (subbytevector buf 0 nbytes)))
           (format #t "Output bytevector contents: ~S~%" bv)
           (format #t "Output bytevector as UTF8: ~S~%" (utf8->string bv))
           (format #t "Bytes read: ~S~%" nbytes)
           (format #t "Error code: ~S~%" status)
           (and (string=?
                 (utf8->string bv)
                 "foobar")
                (equal? 6               ; the number of bytes in 'foobar'
                        nbytes))))))))
