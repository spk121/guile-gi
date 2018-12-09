(use-modules (gi)
	     (test automake-test-lib))

(automake-test
 (begin
   (format #t "Running a GC after loading Glib~%")
   (load-typelib "GLib" "2.0")
   (gc)
   #t))
