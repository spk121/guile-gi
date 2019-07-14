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
(use-modules (gi) (oop goops))
(use-typelibs ("GLib" "2.0")
              ("Gio" "2.0")
              ("Gtk" "3.0")
              ("WebKit2" "4.0"))

(define (activate app)
  (let ((window (application-window:new app))
        (vbox (vbox:new 0 0))
        (browser (web-view:new)))
    (set-title window "Browser")
    (set-default-size window 600 400)
    (add window browser)

    (load-uri browser "http://gnu.org/s/mes")
    (show-all window)))

(define-method (connect obj (signal <symbol>) (handler <procedure>))
  (connect obj (make-signal #:name (symbol->string signal)) handler))

(define (main)
  (let ((app (application:new "org.gtk.example" 0)))
    (connect app 'activate activate)
    (run app (command-line))))

(main)
