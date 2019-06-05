;; mainloop.scm - testing bindings of GLib's main loop
;; Copyright (C) 2018 Michael L. Gran
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Adapted somewhat from GLib's internal mainloop.c test case

(use-modules (gi)
             (lib)
             (rnrs bytevectors)
             (system foreign)
             (ice-9 eval-string)
             (ice-9 rdelim)
             (srfi srfi-9 gnu))

(set-record-type-printer! <GObject> gobject-printer)
(set-record-type-printer! <GBox> gbox-printer)

(let* ((rpt (make-count-reporter))
       (counter-proc (car rpt))
       (results-proc (cadr rpt))
       (ctx #f)
       (source #f)
       (id 0))
  (register-reporter counter-proc)
  (register-reporter full-reporter)

  (with-test-prefix
   "load typelib"

   (pass-if "load GLib 2.0 repository"
            (import-typelib "GLib" "2.0")
            #t))

  (with-test-prefix
   "main context"

   (pass-if "MainContext-new returns a context"
            (set! ctx (MainContext-new))
            (gbox? ctx))

   (pass-if "main context is not pending"
            (not (MainContext-pending? ctx)))

   (pass-if "an iteration of the main loop doesn't dispatch events"
            (not (MainContext-iteration? ctx #f)))

   (pass-if "g_idle_remove_by_data works"
            (let ((callback (lambda (user-data)
                              (format #t "In callback.  Received ~s~%" user-data)
                              #f))
                  (user-data (make-bytevector 8 0)))
              (idle-add PRIORITY_DEFAULT_IDLE
                        callback
                        (bytevector->pointer user-data) #f)
              (idle-remove-by-data (bytevector->pointer user-data)))))

  (flush-all-ports)
  (usleep 100)

  (with-test-prefix
   "mainloop basics"
   (let ((loop #f)
         (ctx #f))

     (pass-if "new main loop is not running"
              (set! loop (MainLoop-new #f #f))
              (not (MainLoop-is-running? loop)))


     (flush-all-ports)
     (usleep 100)
     (expect-fail "main loop context is default context"
              (set! ctx (MainLoop-get-context loop))
              (format #t "main loop context: ~S. default context: ~S.~%" ctx (MainContext-default))
              (equal? ctx (MainContext-default)))

     (flush-all-ports)
     (usleep 100)
     (pass-if "main depth is zero"
              (= (main-depth) 0))

     (flush-all-ports)
     (usleep 100)
     (pass-if "garbage collection doesn't crash"
              (MainLoop-quit loop)
              (set! loop #f)
              (gc)
              #t)
     ))

  (flush-all-ports)
  (usleep 100)

  (with-test-prefix
   "test timeouts"
   (let ((ctx #f)
         (loop #f)
         (source #f)
         (a 0)
         (b 0)
         (c 0))
     (define (a++ data)
       (set! a (1+ a)))
     (define (b++ data)
       (set! b (1+ b)))
     (define (c++ data)
       (set! c (1+ c)))
     (define (stopit data)
       (MainLoop-quit loop))

     (pass-if "setup"
              (let ((sourceA (timeout-source-new 100))
                    (sourceB (timeout-source-new 250))
                    (sourceC (timeout-source-new 330))
                    (sourceD (timeout-source-new 1050)))
                (set! ctx (MainContext-new))
                (set! loop (MainLoop-new ctx #f))
                (Source-set-callback sourceA a++ #f #f)
                (Source-set-callback sourceB b++ #f #f)
                (Source-set-callback sourceC c++ #f #f)
                (Source-set-callback sourceD stopit #f #f)
                (Source-attach sourceA ctx)
                (Source-attach sourceB ctx)
                (Source-attach sourceC ctx)
                (Source-attach sourceD ctx))
              #t)

     (pass-if "timeouts can run"
              (MainLoop-run loop)
              #t)

     (flush-all-ports)
     (usleep 100)
     (pass-if "fast source fired"
              (> a 0))

     (flush-all-ports)
     (usleep 100)
     (pass-if "fast source fired more often than medium source"
              (>= a b))

     (flush-all-ports)
     (usleep 100)
     (pass-if "medium source fired more often than slow source"
              (>= b c))

     (flush-all-ports)
     (usleep 100)
     (pass-if "fast source fired <= 10 times"
              (<= a 10))

     (flush-all-ports)
     (usleep 100)
     (pass-if "medium source fired <= 4 times"
              (<= b 4))

     (flush-all-ports)
     (usleep 100)
     (pass-if "slow source fired <= 3 times"
              (<= c 3))

     (flush-all-ports)
     (usleep 100)
     (pass-if "garbage collection doesn't crash"
              (set! ctx #f)
              (set! loop #f)
              (usleep 1) (gc)
              (usleep 1) (gc)
              (usleep 1) (gc)
              #t)

     ))

  (with-test-prefix
   "test priorities"
   (let ((ctx #f)
         (sourceA #f)
         (sourceB #f)
         (a 0)
         (b 0)
         (c 0))

     (pass-if "setup test, part 1"
              (set! ctx (MainContext-new))
              (set! sourceA (idle-source-new))
              (set! sourceB (idle-source-new))
              #t)

     (flush-all-ports)
     (usleep 100)
     (pass-if "setup test, part 2"
              (Source-set-callback sourceA
                                   (lambda (x)
                                     (set! a (1+ a)))
                                   #f #f)
              (Source-set-priority sourceA 1)
              (Source-attach sourceA ctx)

              (Source-set-callback sourceB
                                   (lambda (x)
                                     (set! b (1+ b)))
                                   #f #f)
              (Source-set-priority sourceB 0)
              (Source-attach sourceB ctx)

              (MainContext-pending? ctx))

     (flush-all-ports)
     (usleep 100)
     (pass-if "iterate once"
              (MainContext-iteration? ctx #f))

     (flush-all-ports)
     (usleep 100)
     (pass-if "low priority callback didn't run"
              (= a 0))

     (flush-all-ports)
     (usleep 100)
     (pass-if "high priority callback did run"
              (= b 1))

     (flush-all-ports)
     (usleep 100)
     (pass-if "iterate once"
              (MainContext-iteration? ctx #f))

     (flush-all-ports)
     (usleep 100)
     (pass-if "low priority callback didn't run"
              (= a 0))

     (flush-all-ports)
     (usleep 100)
     (pass-if "high priority callback did run"
              (= b 2))

     (flush-all-ports)
     (usleep 100)
     (pass-if "garbage collection doesn't crash"
              (set! ctx #f)
              (usleep 1) (gc)
              (set! sourceA #f)
              (usleep 1) (gc)
              (set! sourceB #f)
              (usleep 1) (gc)
              #t)
     ))

  (flush-all-ports)
  (usleep 100)

  (with-test-prefix
   "test invoke"
   (let ((ctx #f)
         (thread #f)
         (count 0))

     (define (func data)
       (set! count (1+ count))
       SOURCE_REMOVE)

     (define (call-func data)
       ;; (func (thread-self))
       (func #f)
       SOURCE_REMOVE)

     (pass-if "invoking a func from idle"
              (set! count 0)
              (idle-add PRIORITY_DEFAULT_IDLE
                        call-func
                        #f
                        #f)
              (MainContext-iteration? (MainContext-default) #f)
              (= count 1))

     (flush-all-ports)
     (usleep 100)
     ))

  (print-counts (results-proc))
  (exit-value (results-proc))
  )
