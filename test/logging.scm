(use-modules (gi logging)
             (ice-9 optargs)
             (srfi srfi-64))

(test-begin "logging")

(define %custom-logger-called 0)
(test-assert "install custom logger"
  (begin
    (install-custom-logger!
     (lambda* (#:key message #:allow-other-keys)
       (with-output-to-port (current-error-port)
         (lambda ()
          (display message)
          (newline)))
       (set! %custom-logger-called (1+ %custom-logger-called))))
    #t))

(test-assert "use modules"
  (begin
    (use-modules (gi))
    (> %custom-logger-called 0)))

(test-end "logging")
