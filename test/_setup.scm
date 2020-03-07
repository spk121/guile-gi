(use-modules (gi) (gi repository)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "_setup.scm")
(test-display "path" (getenv "PATH"))
(test-display "guile:version" (version))
(test-display "cwd" (getcwd))
(test-end "_setup.scm")
