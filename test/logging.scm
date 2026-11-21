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

(test-assert "a logger that raises does not break loading"
  ;; Errors from the logger used to escape into whatever C code was
  ;; logging, aborting it.
  (begin
    (install-custom-logger!
     (lambda* (#:key #:allow-other-keys)
       (error "logger failure")))
    (let ((module ((@ (gi repository) typelib->module) '(test logging gobject)
                   "GObject" "2.0")))
      (install-port-logger! (current-error-port))
      (module? module))))

(test-end "logging")
