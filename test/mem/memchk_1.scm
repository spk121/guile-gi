(use-modules (gi)
	     (test automake-test-lib))

(automake-test
 (begin
   (format #t "Running a GC after loading (gi)~%")
   (gc)
   #t))
