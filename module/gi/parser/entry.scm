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

(define-module (gi parser entry)
  #:use-module (srfi srfi-9)
  #:export (<entry>
            make-entry
            entry?
            get-declaration
            get-definition))

;; A simple type to hold both the declaration info (the export names)
;; and the definition code for an entry
(define-record-type <entry>
  (make-entry declaration               ; list of symbols to #:export
              definition                ; code
              )
  entry?
  (declaration get-declaration)
  (definition get-definition))
