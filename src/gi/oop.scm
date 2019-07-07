;; Copyright (C), 2019 Michael L. Gran

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

(define-module (gi oop)
  #:use-module (oop goops)
  #:export (<property>))

(define-class <property> (<applicable-struct-with-setter>)
  nick)

(define-method (initialize (property <property>) initargs)
  ;; TODO: find a way to pass procedure and setter up
  (next-method)
  (slot-set! property 'procedure
             (lambda (obj)
               ((@ (gi) gobject-get-property)
                obj
                (slot-ref property 'nick))))
  (slot-set! property 'setter
             (lambda (obj val)
               ((@ (gi) gobject-set-property!)
                obj
                (slot-ref property 'nick)
                val)))
  (slot-set! property 'nick
             (get-keyword #:nick initargs #f)))
