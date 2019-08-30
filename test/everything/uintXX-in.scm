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
       (procs (list uint8-in
                    uint16-in
                    uint32-in
                    uint64-in
                    ushort-in
                    uint-in
                    ulong-in)))
   (format #t "Input: ~S~%" x)
   (map (lambda (proc val)
          (proc val))
        procs x)))
