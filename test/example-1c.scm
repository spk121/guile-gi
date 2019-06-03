;; Copyright (C) 2019  Michael L. Gran

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
(use-modules (gi))

(typelib-load "Gio" "2.0")
(typelib-load "Gtk" "3.0")
(typelib-load "GLib" "2.0")

(define (print-hello widget data)
  (display "Hello World\n"))

(define (activate app user-data)
  (let* ((window (make-gobject <GtkApplicationWindow> `(("application" . ,app)
                                                        ("default-height" . 200)
                                                        ("default-width" . 200)
                                                        ("title" . "Window"))))
         (button-box (cast (ButtonBox-new 0) <GtkButtonBox>))
         (button (make-gobject <GtkButton> `(("label" . "Hello World")
                                             ("parent" . ,button-box)))))
    (send window (add button-box))

    (connect button (clicked print-hello #f))
    (connect button (clicked (lambda x
                               (send window (destroy)))
                             #f))
    (send window (show-all))))

(define (main)
  (let ((app
         (make-gobject <GtkApplication> '(("application-id" . "org.gtk.example")))))
    (connect app (activate activate #f))
    (send app (run (length (command-line)) (command-line)))))

(main)
