;; Copyright 2016 Free Software Foundation, Inc.

;; This file is part of Guile-Ncurses.

;; Guile-Ncurses is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; Guile-Ncurses is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with Guile-Ncurses.  If not, see
;; <http://www.gnu.org/licenses/>.

(define-module (test automake-test-lib)
  #:use-module (srfi srfi-1)
  #:export (EXIT_SUCCESS
	    EXIT_FAILURE
	    EXIT_SKIPPED
	    EXIT_HARD_ERROR
	    automake-test
	    maybe-sleep
	    with-latin1-locale*
	    with-utf8-locale*))

(define EXIT_SUCCESS 0)
(define EXIT_FAILURE 1)
(define EXIT_SKIPPED 77)
(define EXIT_HARD_ERROR 99)

(define *sleep* #f)

(define (maybe-sleep n)
  (if (or *sleep*
          (getenv "GUCU_VERBOSE_TEST"))
      (sleep n)))

(define-syntax hard-error-if-exception
  (syntax-rules ()
    ((hard-error-if-exception expr)
     (catch #t
	    (lambda () expr)
	    (lambda args EXIT_HARD_ERROR)))
    ((hard-error-if-exception expr #:warning template arg ...)
     (catch #t
	    (lambda () expr)
	    (lambda (key . args)
	      (for-each (lambda (s)
			  (if (not (string-null? s))
			      (format (current-warning-port) ";;; ~a\n" s)))
			(string-split
			 (call-with-output-string
			  (lambda (port)
			    (format port template arg ...)
			    (print-exception port #f key args)))
			 #\newline))
	      EXIT_HARD_ERROR)))))

(define (automake-test x)
  (let ((ret (hard-error-if-exception x)))
    (cond
     ((eqv? ret EXIT_HARD_ERROR)
      (exit EXIT_HARD_ERROR))
     ((eqv? ret #t)
      (exit EXIT_SUCCESS))
     ((eqv? ret #f)
      (exit EXIT_FAILURE))
     ((eqv? ret 'skipped)
      (exit EXIT_SKIPPED)))))

;;; Call THUNK with a given locale
(define (with-locale* nloc thunk)
  (let ((loc #f))
    (dynamic-wind
	(lambda ()
          (if (defined? 'setlocale)
              (begin
                (set! loc (false-if-exception (setlocale LC_ALL)))
                (if (or (not loc)
                        (not (false-if-exception (setlocale LC_ALL nloc))))
                    (throw 'unresolved)))
              (throw 'unresolved)))
	thunk
	(lambda ()
          (if (and (defined? 'setlocale) loc)
              (setlocale LC_ALL loc))))))

;;; Try out several ISO-8859-1 locales and run THUNK under the one that works
;;; (if any).
(define (with-latin1-locale* thunk)
  (define %locales
    (append-map (lambda (name)
                  (list (string-append name ".ISO-8859-1")
                        (string-append name ".iso88591")
                        (string-append name ".ISO8859-1")))
                '("ca_ES" "da_DK" "de_DE" "es_ES" "es_MX" "en_GB" "en_US"
                  "fr_FR" "pt_PT" "nl_NL" "sv_SE")))

  (let loop ((locales %locales))
    (if (null? locales)
        'skipped
        (catch 'unresolved
          (lambda ()
            (with-locale* (car locales) thunk))
          (lambda (key . args)
            (loop (cdr locales)))))))

;;; Try out several UTF8 locales and run THUNK under the one that works
;;; (if any).
(define (with-utf8-locale* thunk)
  (define %locales
    (append-map (lambda (name)
                  (list (string-append name ".utf8")
                        (string-append name ".UTF-8")
                        (string-append name ".utf-8")
                        (string-append name ".UTF8")
			))
                '("ca_ES" "da_DK" "de_DE" "es_ES" "es_MX" "en_GB" "en_US"
                  "fr_FR" "pt_PT" "nl_NL" "sv_SE")))

  (let loop ((locales %locales))
    (if (null? locales)
        'skipped
        (catch 'unresolved
          (lambda ()
            (with-locale* (car locales) thunk))
          (lambda (key . args)
            (loop (cdr locales)))))))
