(use-modules (gi) (gi repository))

(eval-when (compile load eval)
  (require "Gtk" "3.0")

  (for-each (lambda (n) (load-by-name "Gtk" n))
            '("Builder" "Window" "Grid" "Widget" "Button"
              "init" "main_quit" "main")))

(define (make-builder)
  (let* ((builder (make <GtkBuilder>))
         (window (make <GtkWindow> #:visible #t #:title "Grid"
                       #:border-width 10))
         (grid (make <GtkGrid> #:visible #t #:parent window))
         (button1 (make <GtkButton> #:visible #t #:label "Button 1"))
         (button2 (make <GtkButton> #:visible #t #:label "Button 2"))
         (quit-button (make <GtkButton> #:visible #t #:label "Quit")))
    (attach grid button1 0 0 1 1)
    (attach grid button2 1 0 1 1)
    (attach grid quit-button 0 1 2 1)
    (expose-object builder "window" window)
    (expose-object builder "grid" grid)
    (expose-object builder "button1" button1)
    (expose-object builder "button2" button2)
    (expose-object builder "quit" quit-button)
    builder))

(define (print-hello button)
  (display "Hello World")
  (newline))

(define %builder #f)

(define (go)
  (init!)

  (let ((builder (make-builder)))
    ;; need to keep a global reference or else GC hates us
    (set! %builder builder)
    (let ((window (get-object builder (peek "window")))
          (button1 (get-object builder (peek "button1")))
          (button2 (get-object builder (peek "button2")))
          (button3 (get-object builder (peek "quit"))))
      (connect window destroy
               (lambda (object)
                 (main-quit)))
      (connect button1 clicked print-hello)
      (connect button2 clicked print-hello)
      (connect button3 clicked
               (lambda (button)
                 (main-quit)))))

  (gc) (gc) (gc)
  (main)
  #t)

(go)
