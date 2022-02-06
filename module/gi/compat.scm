;; Copyright (C) 2020, 2022 Michael L. Gran

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>

(define-module (gi compat)
  #:use-module (gi types)
  #:use-module (oop goops)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:export (fiddle dynamic-fiddler))    ; plus 'export' at bottom of file

(define (%fiddle proc obj)
  "Applies PROC, a procedure that takes one argument, to the 'value' slot
of OBJ, an instance of <GFundamental>.  The 'value' slot should
contain a pointer."
  (proc (slot-ref obj 'value)))

(define-method (fiddle (proc <procedure>) (boxed <GBoxed>))
  "Applies PROC, a procedure that takes one argument, to the raw pointer
of BOXED"
  (%fiddle proc boxed))

(define-method (fiddle (proc <procedure>) (object <GObject>))
  "Applies PROC, a procedure that takes one argument, to the raw pointer
of OBJECT."
  (%fiddle proc object))

(define (dynamic-fiddler name lib)
  "Load NAME from LIB and convert it into a procedure. The procedure
takes one <pointer> argument and returns a generic Scheme object."
  (and lib
       (compose pointer->scm
                (pointer->procedure '* (dynamic-func name lib) (list '*)))))

(define %libcairo
  (and-let* ((module (resolve-module '(cairo config) #:ensure #f))
             (libcairo (module-ref module '*cairo-lib-path*)))
    (false-if-exception (dynamic-link libcairo))))

(define context->cairo
  (cute fiddle (dynamic-fiddler "scm_from_cairo" %libcairo) <>))

(define surface->cairo
  (cute fiddle (dynamic-fiddler "scm_from_cairo_surface" %libcairo) <>))

(define font-face->cairo
  (cute fiddle (dynamic-fiddler "scm_from_cairo_font_face" %libcairo) <>))

(define scaled-font->cairo
  (cute fiddle (dynamic-fiddler "scm_from_cairo_scaled_font" %libcairo) <>))

(when %libcairo
  (export context->cairo surace->cairo font-face->cairo scaled-font->cairo))
