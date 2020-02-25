(use-modules (gi) (gi types) (gi repository)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-64))

(require "GLib" "2.0")
(load-by-name "GLib" "IOCondition")
(load-by-name "GLib" "MainLoop")
(load-by-name "GLib" "MainContext")
(load-by-name "GLib" "PRIORITY_DEFAULT")
(load-by-name "GLib" "idle_add")
(load-by-name "GLib" "timeout_add")
(load-by-name "GLib" "unix_fd_add_full")

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

(define unix-fd-sample-text "Lorem ipsum dolor sit amet")
(define unix-fd-result #f)

(test-assert "add unix-fd callback"
  (< 0
     (let* ((ports (pipe))
            (in-port (car ports))
            (out-port (cdr ports)))
       (display unix-fd-sample-text out-port)
       (newline out-port)
       (close out-port)
       (unix-fd-add-full PRIORITY_DEFAULT
                         (port->fdes in-port)
                         (list->iocondition '(hup in))
                         (lambda (fd condition dummy)
                           (cond
                            ((flags-set? condition 'in)
                             (let* ((port (dup->inport fd))
                                    (line (get-line port)))
                               (set! unix-fd-result line)
                               #t))
                            ((flags-set? condition 'hup)
                             ;; close in-port on SIGHUP
                             (close in-port)
                             #f)))
                         #f
                         (const #f)))))

(test-assert "run mainloop"
  (begin
    (run loop)
    #t))

(test-equal "idle ran 5 times" 5 n-idle)
(test-equal "timeout ran 5 times" 5 n-timeout)

(test-equal "unix-fd callback ran"
  unix-fd-sample-text
  unix-fd-result)

(test-end "main-loop")
