(use-modules (gi) (gi repository)
             (test automake-test-lib))

(unless (false-if-exception (require "Gio" "2.0"))
  (exit EXIT_SKIPPED))

(load-by-name "Gio" "Application")

(automake-test
 (begin
   (let ((app (make <GApplication>
                #:application-id "gi.guile.Example"))
         (result #f))
     (connect app activate
              (lambda (app)
                (set! (resource-base-path app)
                      "/gi/guile/resource/base_path")
                (set! result
                      (equal? (resource-base-path app)
                              "/gi/guile/resource/base_path"))
                (quit app)))
     (run app (command-line))
     result)))
