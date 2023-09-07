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

(define-module (gi parser constant)
  #:use-module (gi girepository)
  #:use-module (gi parser entry)
  #:export (constant-info-parents
            constant-info-children
            parse-constant-info))

;; Constants have no prerequisites or GType. They are defined during
;; parsing.

(define (constant-info-parents constant-info namespace)
  (let* ((type-info (constant-info-get-type constant-info))
         (type-tag (type-info-get-tag type-info)))
    (cond
     ((type-tag-is-basic? type-tag)
      '())
     (else
      (scm-error 'misc-error "constant-info-parents"
                 "Unhandled constant type: ~S of type ~S"
                 (list (base-info-get-name constant-info)
                       (base-info-get-name type-info))
                 #f)))))
  
(define (parse-constant-info constant-info)
  (assert-giconstantinfo "parse-constant-info" constant-info)
  (let ((name-symbol (string->symbol (base-info-get-name constant-info)))
        (val (constant-info-get-value constant-info)))
    (list
     (make-entry
      ;; Declaration
      (list name-symbol)

      ;; Definition
      `(define ,name-symbol ,val)))))


(define (constant-info-children constant-info)
  '())
