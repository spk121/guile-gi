(use-modules (gi) (gi util)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-64))

(use-typelibs (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip))))

(define (subbytevector bv start end)
  (let ((bv2 (make-bytevector (- end start))))
    (bytevector-copy! bv start bv2 0 (- end start))
    bv2))

(define-syntax-rule (second-value body)
  (call-with-values (lambda () body)
    (lambda args (second args))))

(test-begin "io")

(test-equal "write chars"
  (symbol->iostatus 'normal)
  (let ((channel (iochannel:new-file "tmp.txt" "w")))
    (iochannel:write-chars channel (string->utf8 "foobar") 6)
    (iochannel:shutdown channel #t)))

(test-assert "read chars content"
  (let ((channel (iochannel:new-file "tmp.txt" "r"))
        (buf (make-bytevector 10 0)))
    (call-with-values (lambda () (iochannel:read-chars! channel buf))
      (lambda (status out nbytes)
        (let ((content (subbytevector out 0 nbytes)))
          (and (equal? status (symbol->iostatus 'normal))
               (= nbytes 6)
               (string=? (utf8->string content) "foobar")))))))

(test-assert "chars cleanup"
  (delete-file "tmp.txt"))

(test-assert "write unichar"
  (let ((channel (iochannel:new-file "tmp.txt" "w")))
    (iochannel:write-unichar channel #\α)
    (iochannel:write-unichar channel #\β)
    (iochannel:shutdown channel #t)))

(test-assert "read unichar"
  (let* ((channel (iochannel:new-file "tmp.txt" "r"))
         (alpha (second-value (iochannel:read-unichar channel)))
         (beta  (second-value (iochannel:read-unichar channel))))
    (and
     (equal? alpha #\α)
     (equal? beta #\β))))

(test-assert "unichar cleanup"
  (delete-file "tmp.txt"))

(let* ((ports (pipe))
       (in-port (car ports))
       (out-port (cdr ports)))
  (test-assert "write to pipe"
    (begin
      (display "hello" out-port)
      (close out-port)))

  (test-assert "channel-read from pipe"
    (let ((channel (iochannel:unix-new (port->fdes in-port)))
          (buf (make-bytevector 10 0)))
      (call-with-values (lambda () (iochannel:read-chars! channel buf))
        (lambda (status out nbytes)
          (let ((content (subbytevector buf 0 nbytes))
                (out (subbytevector out 0 nbytes)))
            (and (equal? status (symbol->iostatus 'normal))
                 (= nbytes 5)
                 (string=? (utf8->string content) "hello"
                           (utf8->string out)))))))))

(test-end "io")
