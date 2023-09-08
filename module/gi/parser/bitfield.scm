;; Copyright (C) 2023 Michael L. Gran

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

(define-module (gi parser bitfield)
  #:use-module (gi girepository)
  #:use-module (gi parser entry)
  #:export (bitfield-info-parents
            parse-bitfield-info
            bitfield-info-children))

(define (bitfield-values->alist info)
  (assert-gienuminfo "bitfield-values->alist" info)
  (map-in-order
   (lambda (x)
     (cons (base-info-get-name x) (value-info-get-value x)))
   (enum-info-get-values info)))

(define (parse-untyped-bitfield-info info)
  (let ((name-symbol (string->symbol (base-info-get-name info)))
        (values-alist (bitfield-values->alist info)))
    (list
     (make-entry
      ;; Declaration.
      (list name-symbol)
      `(define-class ,name-symbol (<GUntypedBitfield>)
         (values #:allocation #:class
                 #:init-value ,values-alist))))))

(define (parse-typed-bitfield-info info)
  (let ((name-symbol (string->symbol (base-info-get-name info)))
        (gtype-string (registered-type-info-get-type-name info)))
    (list
     (make-entry
      ;; Declaration.
      (list name-symbol)
      `(define-class ,name-symbol (<GBitfield>)
         (gtype-string #:allocation #:class #:initvalue ,gtype-string))))))

(define (bitfield-info-parents bitfield-info)
  '())

(define (parse-bitfield-info bitfield-info namespace)
  (assert-gienuminfo "parse-bitfield-info" bitfield-info)

  ;; If a bitfield has a GType, its name->value mapping is a runtime concern.
  ;; If it has no GType, the name->value mapping is created now.
  (if (registered-type-info-has-g-type? bitfield-info)
      (parse-typed-bitfield-info bitfield-info)
      (parse-untyped-bitfield-info bitfield-info)))

(define (bitfield-info-children bitfield-info)
  (enum-info-get-methods bitfield-info))
