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
(use-modules (gi)
             (ice-9 receive))

(typelib-load "Gio" "2.0")
(typelib-load "Gdk" "3.0")
(typelib-load "Gtk" "3.0")
(typelib-load "GLib" "2.0")
(typelib-load "WebKit2" "4.0")

;; Oddly, the introspection information does not provide a constructor
;; for GtkTextIter.
(define (text-iter:new)
  (make-gstruct <GtkTextIter>))

(define (print-goodbye widget)
  (display "Goodbye World\n"))

(define (key-press widget event)
  (receive (ok keyval)
      (with-object event (get-keyval?))
      (format #t "key: ~s\n" keyval)
      #f))

(define (activate app)
  (let ((window (cast (application-window:new app) <GtkApplicationWindow>))
        (vbox (cast (vbox:new 0 0) <GtkVBox>))
        (editor (cast (text-view:new) <GtkTextView>))
        (button-box (cast (button-box:new 0) <GtkButtonBox>))
        (button (button:new-with-label "Quit"))
        (button2 (button:new-with-label "Hello")))
    (with-object editor (add-events EVENT_MASK_KEY_PRESS_MASK))
    (with-object button-box (add button2) (add button))
    (with-object vbox (add editor) (add button-box))
    (with-object window
      (set-title "Window")
      (set-default-size 200 200)
      (add vbox))

    (modify-signals button
      (connect clicked print-goodbye)
      (connect clicked (lambda x
                         (with-object window (destroy)))))

    (modify-signals editor (connect key-press-event key-press))

    ;; When the 'hello' button is clicked, write the current contents
    ;; of the editor to the console, and replace the buffer contents
    ;; with 'Hello, world'.
    (modify-signals button2
      (connect clicked (lambda x
                         (let ((buffer (with-object editor (get-buffer)))
                               (iter1 (text-iter:new))
                               (iter2 (text-iter:new)))
                           (with-object buffer (get-bounds iter1 iter2))
                           (let ((txt (with-object buffer (get-text iter1 iter2 #t))))
                             (write txt) (newline))
                           (with-object buffer (set-text "Hello, world" 12))))))

    (with-object editor (grab-focus))
    (with-object window (show-all))))

(define (main)
  (let ((app (application:new "org.gtk.example" 0)))
    (modify-signals app (connect activate activate))
    (with-object app (run (length (command-line)) (command-line)))))

(main)
