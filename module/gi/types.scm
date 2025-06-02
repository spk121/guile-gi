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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (gi types)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)

  #:use-module (gi runtime)

  #:re-export (<GBoxed> <GObject>
               <GValue> transform
               <GClosure> procedure->closure
               <GEnum> <GFlags>
               enum->number enum->symbol number->enum symbol->enum
               flags->number flags->list number->flags list->flags flags-set?
               enum-universe
               flags-mask flags-union flags-intersection flags-difference
               flags-complement flags-projection flags-projection/list
               flags-projection/number
               is-registered-callback?
               get-registered-callback-closure-pointer

               gtype-get-name
               gtype-get-scheme-type
               gtype-get-parent
               gtype-is-a?
               gtype-get-children
               gtype-get-fundamental
               %gtype-dump-table
               gtype-get-interfaces
               gtype-get-depth
               gtype-is-interface?
               gtype-is-classed?
               gtype-is-instantiatable?
               gtype-is-derivable?))
