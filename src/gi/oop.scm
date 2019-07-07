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
  #:export (<property>
            <number-property>

            G_PARAM_READABLE
            G_PARAM_WRITABLE
            G_PARAM_READWRITE
            G_PARAM_CONSTRUCT
            G_PARAM_CONSTRUCT_ONLY))

(eval-when (expand load eval)
  ;; required for %typelib-module-name, which is used at expand time
  (load-extension "libguile-gi" "gi_init_gparamspec_private"))

(define-class <property> (<applicable-struct-with-setter>)
  name type nick blurb flags default)

(define-method (initialize (property <property>) initargs)
  ;; TODO: find a way to pass procedure and setter up
  (next-method)
  (slot-set! property 'procedure
             (lambda (obj)
               ((@ (gi) gobject-get-property)
                obj
                (slot-ref property 'name))))
  (slot-set! property 'setter
             (lambda (obj val)
               ((@ (gi) gobject-set-property!)
                obj
                (slot-ref property 'name)
                val)))
  (slot-set! property 'name (get-keyword #:name initargs #f))
  (slot-set! property 'type (get-keyword #:type initargs 0)) ; should be G_TYPE_INVALID
  (slot-set! property 'nick (get-keyword #:nick initargs (slot-ref property 'name)))
  (slot-set! property 'blurb (get-keyword #:blurb initargs #f))
  (slot-set! property 'flags (get-keyword #:flags initargs G_PARAM_READWRITE))
  (slot-set! property 'default (get-keyword #:default initargs #f)))

(define-class <number-property> (<property>)
  min max)

(define-method (initialize (property <number-property>) initargs)
  (next-method)
  (slot-set! property 'min (get-keyword #:min initargs #f))
  (slot-set! property 'max (get-keyword #:max initargs #f))
  (slot-set! property 'default (get-keyword #:default initargs 0)))
