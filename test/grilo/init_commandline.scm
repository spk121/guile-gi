(use-modules (gi)
             (test automake-test-lib))

(typelib-require ("Grl" "0.3"))

(automake-test
 (let ((cmdline (command-line)))
   (format #t "Command line: ~S~%" cmdline)
   (let ((init-status (init cmdline)))
     (format #t "Init status: ~S~%" init-status)
     init-status)))
