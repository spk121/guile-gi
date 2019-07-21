;; Copyright (C) 2019  Michael L. Gran
;; Copyright (C) 2019  Jan (janneke) Nieuwenhuizen <janneke@gnu.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https:;;www.gnu.org/licenses/>.
(use-modules (gi) (gi util)
             (oop goops)
             (ice-9 receive))

(push-duplicate-handler! 'shrug-equals)

(use-typelibs (("Gio" "2.0") #:renamer (protect 'receive))
              ("Gdk" "3.0")
              (("Gtk" "3.0") #:renamer (protect %rnrs-syntax))
              ("GLib" "2.0"))

(define (print-goodbye widget)
  (display "Goodbye World\n"))

(define (key-press widget event)
  (receive (ok keyval)
      (get-keyval event)
    (format #t "key: ~s\n" keyval)
    #f))

(define (activate app)
  (let ((window (application-window:new app))
        (vbox (vbox:new 0 0))
        (editor (text-view:new))
        (button-box (button-box:new 0))
        (button (button:new-with-label "Quit"))
        (button2 (button:new-with-label "Hello")))
    (add-events editor EVENT_MASK_KEY_PRESS_MASK)

    (map add
         (list button-box button-box vbox vbox window)
         (list button2 button editor button-box vbox))

    (set-title window "Window")
    (set-default-size window 200 200)

    (map connect
         (list editor button button button2)
         (list key-press-event clicked clicked clicked)
         (list key-press
               print-goodbye (lambda x (destroy window))
               ;; When the 'hello' button is clicked, write the current contents
               ;; of the editor to the console, and replace the buffer contents
               ;; with 'Hello, world'.
               (lambda x
                 (let ((buffer (get-buffer editor))
                       (iter1 (make <GtkTextIter>))
                       (iter2 (make <GtkTextIter>)))
                   (get-bounds buffer iter1 iter2)
                   (write (get-text buffer iter1 iter2 #t))
                   (newline)
                   (set-text buffer "Hello, world" 12)))))

    (grab-focus editor)
    (show-all window)))

(define (main)
  (let ((app (application:new "org.gtk.example" 0)))
    (connect app application:activate activate)
    (run app (command-line))))

(main)
