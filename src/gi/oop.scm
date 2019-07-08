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
            <signal>

            connect connect-after

            G_PARAM_READABLE
            G_PARAM_WRITABLE
            G_PARAM_READWRITE
            G_PARAM_CONSTRUCT
            G_PARAM_CONSTRUCT_ONLY

            G_SIGNAL_RUN_FIRST
            G_SIGNAL_RUN_LAST
            G_SIGNAL_RUN_CLEANUP
            G_SIGNAL_NO_RECURSE
            G_SIGNAL_DETAILED
            G_SIGNAL_ACTION
            G_SIGNAL_NO_HOOKS
            G_SIGNAL_MUST_COLLECT
            G_SIGNAL_DEPRECATED))

(eval-when (expand load eval)
  ;; required for %typelib-module-name, which is used at expand time
  (load-extension "libguile-gi" "gi_init_gobject_private"))

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

(define-class <signal> (<applicable-struct>)
  name flags accumulator return-type param-types)

(define-method (initialize (signal <signal>) initargs)
  (next-method)
  (slot-set! signal 'procedure
             (lambda (obj . args)
               (apply
                (@ (gi) signal-emit)
                obj
                (slot-ref signal 'name)
                args)))

  (slot-set! signal 'name (get-keyword #:name initargs #f))
  (slot-set! signal 'flags (get-keyword #:flags initargs 0))
  (slot-set! signal 'accumulator (get-keyword #:accumulator initargs #f))
  (slot-set! signal 'return-type (get-keyword #:return-type initargs 0))
  (slot-set! signal 'param-types (get-keyword #:param-types initargs '())))

(define-method (connect obj (signal <signal>) (handler <procedure>))
  ((@ (gi) signal-connect) obj (slot-ref signal 'name) handler))

(define-method (connect obj (signal <signal>) (detail <string>) (handler <procedure>))
  ((@ (gi) signal-connect)
   obj
   (string-append (slot-ref signal 'name) "::" detail)
   handler))

(define-method (connect-after obj (signal <signal>) (handler <procedure>))
  ((@ (gi) signal-connect) obj (slot-ref signal 'name) handler #t))

(define-method (connect-after obj (signal <signal>) (detail <string>) (handler <procedure>))
  ((@ (gi) signal-connect)
   obj
   (string-append (slot-ref signal 'name) "::" detail)
   handler
   #t))
