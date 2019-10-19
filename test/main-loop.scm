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
