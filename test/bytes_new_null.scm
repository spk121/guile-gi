;; bytes_new_null.scm
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

(use-modules (gi)
	     (rnrs bytevectors)
	     (oop goops)
             (test automake-test-lib)
	     (srfi srfi-9 gnu))

(setlocale LC_ALL "")
(automake-test
 (begin
   (set-record-type-printer! <GBox> gbox-printer)
   (import-typelib "GLib" "2.0")
   (let ((B (Bytes-new #f 0)))
     (format #t "New Bytes: ~S~%" B)
     (and (is-a? B )
	  (= 0 (bytevector-length B))))))
