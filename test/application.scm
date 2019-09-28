(use-modules (gi) (gi repository)
             (srfi srfi-1)
             (srfi srfi-64))

(require "GLib" "2.0")
(load-by-name "GLib" "MainLoop")
(load-by-name "GLib" "MainContext")
(load-by-name "GLib" "PRIORITY_DEFAULT")
(load-by-name "GLib" "idle_add")
(load-by-name "GLib" "timeout_add")

(define loop #f)

(test-begin "main-loop")

(test-assert "new MainLoop"
  (begin
    (set! loop (main-loop:new #f #t))
    (is-a? loop <GMainLoop>)))

(test-assert "is running" (is-running? loop))
(test-assert "get context" (is-a? (get-context loop) <GMainContext>))

(define %idle-counter 0)

(define on-idle-quit
  (let ((%idle-quit-counter 0))
    (lambda _
      (set! %idle-quit-counter (1+ %idle-quit-counter))
      (when (= %idle-counter %idle-quit-counter)
        (quit loop))
      #f)))

(define n-idle 0)
(define n-timeout 0)

(test-assert "add idle"
  (idle-add PRIORITY_DEFAULT
            (lambda _
              (if (= n-idle 5)
                  #f
                  (begin (set! n-idle (1+ n-idle)) #t)))
            #f
            on-idle-quit))

(if (test-passed?)
    (set! %idle-counter (1+ %idle-counter))
    (test-expect-fail "idle ran 5 times"))

(test-assert "add timeout"
  (timeout-add PRIORITY_DEFAULT
              100
              (lambda _
                (if (= n-timeout 5)
                    #f
                    (begin (set! n-timeout (1+ n-timeout)) #t)))
              #f
              on-idle-quit))

(if (test-passed?)
    (set! %idle-counter (1+ %idle-counter))
    (test-expect-fail "timeout ran 5 times"))

(test-assert "run mainloop"
  (begin
    (run loop)
    #t))

(test-equal "idle ran 5 times" 5 n-idle)
(test-equal "timeout ran 5 times" 5 n-timeout)

(test-end "main-loop")

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
              #:application-id "gi.guile.Example"
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
  (for-each (lambda (fn i) (fn app activate (%fire-signal i)))
               (list connect connect-after connect)
               '(1 2 3)))

(test-assert "connect shutdown signal"
  (connect app shutdown
           (lambda _ (set! %fired-signals (reverse %fired-signals)))))

(test-assert "add options"
  (add-main-option app "option" (char->integer #\o)
                   (number->option-flags 0)
                   (symbol->option-arg 'string-array) "" #f))

(define %command-line-args #f)
(define %command-line-options #f)

(test-assert "connect command-line signal"
  (connect app command-line
           (lambda (app command-line)
             (set! %command-line-args (get-arguments command-line))
             (set! %command-line-options
               (variant:get-strv
                (lookup-value (get-options-dict command-line) "option" #f)))
             (activate app)
             (quit app))))

(test-assert "run"
  (begin
    (application:run app #("hello" "world"
                           "-o" "1" "-o" "2" "-o" "3"))
    #t))

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
