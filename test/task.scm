(use-modules (gi) (gi types) (gi repository) (gi util)
             (oop goops)
             (system foreign)
             (ice-9 format)
             (srfi srfi-64))

(use-typelibs (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip)))
              ("Gio" "2.0"))

(test-begin "task")

(define loop #f)

(test-assert "new MainLoop"
  (begin (set! loop (main-loop:new #f #t))
         (is-a? loop <GMainLoop>)))

(test-assert "create a task"
  (let ((T (task:new)))
    (task:is-valid? T)))

(test-assert "create a task acting on an object"
  (let* ((obj (make <GObject>))
         (T (task:new obj)))
    (task:is-valid? T obj)))

(test-assert "create a task with a Cancellable"
  (let* ((kancel (cancellable:new))
         (T (task:new #f kancel)))
    (task:is-valid? T)))

(test-equal "return pointer from a task"
  #x1234
  (let ((T (task:new)))
    (idle-add PRIORITY_DEFAULT
              (lambda (data)
                (return-pointer T (make-pointer #x1234))
                (quit loop)
                #f))
    (main-loop:run loop)
    (pointer-address (propagate-pointer T))))

(test-equal "return pointer from a task via its callback"
  #x1234
  (let* ((return-value #f)
         (iter 0)
         (T (task:new
             #f
             #f
             (lambda (source task data)
               (set! return-value (pointer-address (propagate-pointer task)))))))
    (idle-add PRIORITY_DEFAULT
              (lambda (data)
                (when (zero? iter)
                  (return-pointer T (make-pointer #x1234)))
                (set! iter (1+ iter))
                (if return-value
                    (begin
                      (quit loop)
                      #f)
                    ;; else
                    #t)))
    (main-loop:run loop)
    return-value))

#; (unless (defined? 'run-in-thread)
    (test-skip 1))

#; (test-equal "return pointer from a task run in thread"
  #x1234
  (let ((T (task:new (make <GObject>))))
    (run-in-thread T
                   (lambda (task source data kancel)
                     (return-pointer T (make-pointer #x1234))))
    (idle-add PRIORITY_DEFAULT
              (lambda (data)
                (if (get-completed? T)
                    (begin
                      (quit loop)
                      #f)
                    ;; else
                    #t)))
    (main-loop:run loop)
    (pointer-address (propagate-pointer T))))

(test-assert "cancel task and check cancellable"
  (let* ((kancel (cancellable:new))
         (T (task:new #f kancel)))
    (idle-add PRIORITY_DEFAULT
              (lambda (data)
                (cancel kancel)
                (quit loop)
                #f))
    (main-loop:run loop)
    (is-cancelled? kancel)))

(test-assert "cancel task and check task was cancelled"
  (let* ((kancel (cancellable:new))
         (T (task:new #f kancel)))
    (idle-add PRIORITY_DEFAULT
              (lambda (data)
                (cancel kancel)
                (quit loop)
                #f))
    (main-loop:run loop)
    (return-error-if-cancelled? T)))


(test-equal "attach pointer to a task and retrieve"
      #xBEEF
      (let ((T (task:new)))
        (set-task-data T (make-pointer #xBEEF))
        (pointer-address (get-task-data T))))

(test-assert "attach pointer and DestroyNotify to a task"
  (let ((T (task:new))
        (destroyed #f))
    ;; Add some data, plus a lambda to be called when the data
    ;; is overwritten.
    (set-task-data T
                   (make-pointer #xFACE)
                   (lambda (ptr)
                     (set! destroyed #t)))
    ;; Overwrite the data.
    (set-task-data T
                   (make-pointer #xD00D))
    destroyed))

(test-end "task")
