(use-modules (gi) (gi repository)
             (srfi srfi-1)
             (srfi srfi-64))

(require "GLib" "2.0")

(test-begin "application")

(define load-by-name?
  (compose
   (negate null?)
   load-by-name))

(if (false-if-exception (require "Gio" "2.0"))
    (test-assert "load Application"
      (load-by-name? "Gio" "Application"))
    (begin
      (test-expect-fail "load Application")
      (test-assert "load Application" #f)))

(unless (test-passed?)
  (test-skip most-positive-fixnum))

(define app #f)

(test-assert "load Application-related stuff"
  (every load-by-name?
         '("GLib" "GLib" "GLib" "GLib"
           "Gio" "Gio")
         '("Variant" "VariantDict" "OptionArg" "OptionFlags"
           "ApplicationCommandLine" "ApplicationFlags")))

(test-assert "make Application"
  (set! app (make <GApplication>
              #:application-id "gi.guile.Example2"
              #:flags '(handles-command-line))))

(let ((path "/gi/guile/resource/base_path"))
  (test-equal "resource base path"
    path
    (begin
      (set! (resource-base-path app) path)
      (resource-base-path app))))

(define %fired-signals '())
(define (%fire-signal arg)
  (lambda _
    (set! %fired-signals (cons arg %fired-signals))))

(test-assert "connect some activation signals"
  (begin
    (connect app activate (%fire-signal 1))
    (connect-after app activate (%fire-signal 2))
    (connect app activate (%fire-signal 3))))

(test-assert "connect shutdown signal"
  (connect app shutdown
           (lambda _ (set! %fired-signals (reverse %fired-signals)))))

(test-assert "add options"
  (add-main-option app "option" (char->integer #\o)
                   (number->option-flags 0)
                   (symbol->option-arg 'string-array) "" #f))

(define %command-line-args #f)
(define %command-line-options #f)

(define (do-command-line app command-line)
  (set! %command-line-args (get-arguments command-line))
  (set! %command-line-options
        (variant:get-strv
         (lookup-value (get-options-dict command-line) "option" #f)))
  (activate app)
  (quit app)
  0)

(test-assert "connect command-line signal"
  (connect app command-line do-command-line))

(test-assert "run"
  (run app #("hello" "world" "-o" "1" "-o" "2" "-o" "3")))

(test-equal "command-line arguments"
  #("hello" "world")
  %command-line-args)

(test-equal "command-line arguments"
  #("1" "2" "3")
  %command-line-options)

(test-equal "signals fired correctly"
  '(1 3 2)
  %fired-signals)

(test-end "application")
