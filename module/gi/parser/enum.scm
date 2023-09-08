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

(define-module (gi parser enum)
  #:use-module (gi girepository)
  #:use-module (gi parser entry)
  #:export (enum-info-parents
            parse-enum-info
            enum-info-children))

(define (enum-values->alist info)
  (assert-gienuminfo "enum-values->alist" info)
  (map-in-order
   (lambda (x)
     (cons (base-info-get-name x) (value-info-get-value x)))
   (enum-info-get-values info)))

(define (parse-untyped-enum-info info)
  (let ((name-symbol (string->symbol (base-info-get-name info)))
        (values-alist (enum-values->alist info)))
    (list
     (make-entry
      ;; Declaration.
      (list name-symbol)
      `(define-class ,name-symbol (<GUntypedEnum>)
         (values #:allocation #:class
                 #:init-value ,values-alist))))))

(define (parse-typed-enum-info info)
  (let ((name-symbol (string->symbol (base-info-get-name info)))
        (gtype-string (registered-type-info-get-type-name info)))
    (list
     (make-entry
      ;; Declaration.
      (list name-symbol)
      `(define-class ,name-symbol (<GEnum>)
         (gtype-string #:allocation #:class #:initvalue ,gtype-string))))))

(define (enum-info-parents enum-info)
  '())

(define (parse-enum-info enum-info namespace)
  (assert-gienuminfo "parse-enum-info" enum-info)

  ;; If an enum has a GType, its name->value mapping is a runtime concern.
  ;; If it has no GType, the name->value mapping is created now.
  (if (registered-type-info-has-g-type? enum-info)
      (parse-typed-enum-info enum-info)
      (parse-untyped-enum-info enum-info)))

(define (enum-info-children enum-info)
  (enum-info-get-methods enum-info))
