;;;; test-runner.scm - Guile test runner for Meson

(define script-version "2020-03-02") ;UTC

;;; Adapted from Automake's test-driver.scm
;;; Copyright © 2015-2018 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules
 (ice-9 curried-definitions)
 (ice-9 getopt-long)
 (ice-9 match)
 (ice-9 optargs)
 (ice-9 pretty-print)
 (srfi srfi-26)
 (srfi srfi-64))

(define (test-runner-meson log-port)
  (define (%test-display field value)
    (format log-port "~A: ~A~%" field value))

  (define (%test-write field value)
    (format log-port "~A: ~S~%" field value))

  (define (%test-display-pretty field value)
    (format log-port "~A:~%" field)
    (pretty-print value log-port #:per-line-prefix "+ "))

  (define* (test-display runner field #:key pretty?)
    (let ((value (test-result-ref runner field *unspecified*))
          (%display (if pretty? %test-display-pretty %test-write)))
      (unless (unspecified? value)
        (%display field value))))

  (define result->string
    (compose string-upcase symbol->string))

  (define (test-on-test-begin-meson runner)
    (%test-display 'test-name (test-result-ref runner 'test-name))
    (format log-port "location: ~A:~A~%"
            (test-result-ref runner 'source-file)
            (test-result-ref runner 'source-line))
    (%test-display-pretty 'source
                          (test-result-ref runner 'source-form)))

  (define (test-on-test-end-meson runner)
    (%test-display 'result (result->string (test-result-kind runner)))
    (test-display runner 'expected-value)
    (test-display runner 'expected-error)
    (test-display runner 'actual-value)
    (test-display runner 'actual-error)
    (test-display runner 'result)
    (newline log-port))

  (let ((runner (test-runner-null)))
    (test-runner-on-test-begin! runner test-on-test-begin-meson)
    (test-runner-on-test-end! runner test-on-test-end-meson)
    (test-runner-on-group-end! runner test-on-group-end-simple)
    (test-runner-on-bad-end-name! runner test-on-bad-end-name-simple)
    runner))

(define (show-help)
  (display "Usage: test-runner.scm TEST-SCRIPT [TEST-SCRIPT-ARGUMENTS]"))

(match (command-line)
  ((this)
   (show-help))
  ((this script . args)
   (let* ((test (basename script ".scm"))
          (log (open-file (string-append test ".log") "w0"))
          (runner (test-runner-meson log)))
     (test-with-runner runner (primitive-load script))
     (exit
      (if (= 0 (+ (test-runner-fail-count runner)
                  (test-runner-xpass-count runner)))
          EXIT_SUCCESS
          EXIT_FAILURE)))))
