(use-modules (gi)
             (test automake-test-lib)
             (srfi srfi-1)
             (system foreign))

(typelib-require (("Marshall" "1.0")
                  #:renamer (protect* '(sizeof short int long size_t))))

(automake-test
 (let ((x (list (1- (expt 2 8))
                (1- (expt 2 16))
                (1- (expt 2 32))
                (1- (expt 2 64))
                (1- (expt 2 (* 8 (sizeof short))))
                (1- (expt 2 (* 8 (sizeof int))))
                (1- (expt 2 (* 8 (sizeof long))))))
       (expected (list 0 0 0 0 0 0 0))
       (procs (list uint8-inout
                    uint16-inout
                    uint32-inout
                    uint64-inout
                    ushort-inout
                    uint-inout
                    ulong-inout)))
   (format #t "Input Before: ~S~%" x)
   (let ((y (map (lambda (proc val)
                   (proc val))
                 procs x)))
     (format #t "Input After: ~S~%" x)
     (format #t "Output: ~S~%" y)
     (format #t "Expected output: ~S~%" expected)
     (list= = y expected))))
