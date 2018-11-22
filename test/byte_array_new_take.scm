;; byte_array_new.scm - testing bindings of GLib's array functions
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

;; Adapted somewhat from GLib's internal array-test.c test case

(use-modules (gi)
	     (rnrs bytevectors)
             (test automake-test-lib))

(setlocale LC_ALL "")
(automake-test
 (begin
   (import-typelib "GLib" "2.0")
   (let* ((bv (make-bytevector 4 32))
	  (bv2 (ByteArray-new-take bv 3)))
     (format #t "Input byte array should be 4 bytes: ~S~%" bv)
     (format #t "Output byte array should be 3 bytes: ~S~%" bv2)
     (and (bytevector? bv2)
	  (= 3 (bytevector-length bv2))))))
