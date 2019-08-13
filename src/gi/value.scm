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

(define-module (gi value)
  #:use-module (gi types)
  #:use-module ((gi repository) #:prefix repo:)

  #:export (init-value-type
            value->accessor
            ;; we should wrap the following in our own procedures
            ;; for documentation's sake
            (%get-type . get-type)
            (%set-type! . set-type!)
            (%transform . transform)))

(eval-when (expand load eval)
  (load-extension "libguile-gi" "gig_init_value"))

(define (init-value-type)
  (repo:require "GObject")
  (repo:load
   (repo:info "GObject" "Value")
   repo:LOAD_INFO_ONLY))

(define (value->accessor value)
  (make-procedure-with-setter
   (lambda () (%get value))
   (case-lambda
    ((val) (%set! value val))
    ((type val)
     (%set-type! value type)
     (%set! value val)))))
