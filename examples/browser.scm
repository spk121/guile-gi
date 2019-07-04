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
(use-modules (gi))

(typelib-load "Gio" "2.0")
(typelib-load "Gtk" "3.0")
(typelib-load "GLib" "2.0")
(typelib-load "WebKit2" "4.0")

(define (print-hello widget)
  (display "Hello World\n"))

(define (activate app)
  (let ((window (cast (application-window:new app) <GtkApplicationWindow>))
        (vbox (cast (vbox:new 0 0) <GtkVBox>))
        (browser (cast (web-view:new) <WebKitWebView>))
        (button-box (cast (button-box:new 0) <GtkButtonBox>))
        (button (button:new-with-label "Hello World")))
    (with-object button-box (add button))
    (with-object vbox (add browser) (add button-box))
    (with-object window
      (set-title "Window")
      (set-default-size 200 200)
      (add vbox)
      (show-all))

    (with-object button
      (connect! clicked print-hello)
      (connect! clicked (lambda x
                          (with-object window (destroy)))))
    (with-object browser (load-uri "http://gnu.org/s/mes"))))

(define (main)
  (with-object (application:new "org.gtk.example" 0)
    (connect! activate activate)
    (run (length (command-line)) (command-line))))

(main)
