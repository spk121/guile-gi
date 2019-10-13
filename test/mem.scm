(use-modules (gi) (gi repository)
             (system foreign)
             (rnrs bytevectors)
             (srfi srfi-64))

(test-begin "mem")

(test-assert "GC after use-modules"
  (begin (gc) #t))

(test-assert "GC after loading typelibs"
  (begin
    (typelib->module (current-module) "GLib" "2.0")
    (gc)
    #t))

(test-assert "malloc"
  (let ((memptr (malloc MAXINT16)))
    (let ((bv (pointer->bytevector memptr MAXINT16)))
      ;; Can we write here?
      (bytevector-u8-set! bv 0 123))
    ;; Can we free this stuff?
    (gc)
    (free memptr)
    #t))

(test-equal "try-malloc0 too much memory"
  #f
  (try-malloc0 MAXINT64))

(test-assert "slice-alloc"
  (let* ((size 16)
         (memptr (slice-alloc size)))
    (let ((bv (pointer->bytevector memptr size)))
      ;; Can we write here?
      (bytevector-u8-set! bv 0 123))
    ;; Can we free this stuff?
    (gc)
    (slice-free1 size memptr)
    #t))

(test-end "mem")
