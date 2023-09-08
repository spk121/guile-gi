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

(define-module (gi parser union)
  #:use-module (gi girepository)
  #:use-module (gi parser entry)
  #:use-module (gi parser field)
  #:export (union-info-parents
            parse-union-info
            union-info-children))

;; Unions have no prerequisites.  They may have a GType.
;; The field information needs to be gathered at parse time.
;; They may have methods.

(define (union-info-children info)
  '())
         
(define (extract-field-info union-info)
  (assert-giunioninfo "extract-field-info" union-info)
  (let ((field-infos (union-info-get-fields union-info)))
    (map-in-order field-info-il field-infos)))
  
(define (parse-union-info union-info namespace)
  (assert-giunioninfo "parse-union-info" union-info)
  (let ((name-symbol (string->symbol (base-info-get-name union-info)))
        (fields (extract-field-info union-info)))

    (list
     (make-entry
      ;; Declaration
      (list name-symbol)

      ;; Definition
      `(define-class ,name-symbol (<GUnion>)
         (field-info #:allocation #:each-subclass
                     #:init-value ,fields))
      ))))


(define (union-info-children union-info)
  (assert-giunioninfo "parse-union-info" union-info)
  (union-info-get-methods union-info))
