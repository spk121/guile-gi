;; Copyright 2016, 2019 Free Software Foundation, Inc.

;; This is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;; Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this file.  If not, see
;; <http://www.gnu.org/licenses/>.

(define-module (test automake-test-lib)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:replace (EXIT_SUCCESS
             EXIT_FAILURE)
  #:export (EXIT_SKIPPED
            EXIT_HARD_ERROR

            automake-test
            tap:test! tap:expect-fail
            tap:finish!

            maybe-sleep
            with-latin1-locale*
            with-utf8-locale*
            typelib-require
            int-vector->list
            list->int-vector
            short-vector->list
            list->short-vector
            long-vector->list
            list->long-vector))

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

(define tap:expect-fail (make-parameter #f (lambda (x) (not (not x)))))

(define %tap-tests '())

(define-record-type <tap-test>
  (make-test name procedure expect-fail?)
  tap-test?
  (name test-name)
  (procedure test-procedure)
  (expect-fail? test-expect-fail?))

(define-syntax-rule (tap:test! test body ...)
  (set! %tap-tests
        (cons (make-test test (lambda () body ...) (tap:expect-fail))
              %tap-tests)))

(define (tap:finish!)
  (let ((fails #f)
        (errors #f))
    (format #t "1..~d~%" (length %tap-tests))
    (for-each
     (lambda (test)
       (format
        #t
        (case (catch #t
                (test-procedure test)
                (const 'error))
          ((#f)
           (if (test-expect-fail? test)
               "not ok ~a # TODO implement"
               (begin
                 (set! fails #t)
                 "not ok ~a")))
          ((skipped)
           "ok ~a # SKIP this test")
          ((error)
           (set! errors #t)
           "Bail out! ~a")
          (else
           (if (test-expect-fail? test)
               (begin
                 (set! fails #t)
                 "ok ~a # TODO when was this added?")
               "ok ~a")))
        (test-name test))
       (newline))
     (reverse %tap-tests))
    (exit
     (cond
      (errors EXIT_HARD_ERROR)
      (fails EXIT_FAILURE)
      (else EXIT_SUCCESS)))))

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

(define-syntax typelib-require
  (lambda (stx)
    (syntax-case stx ()
      ((_ lib ...)
       #'(begin
           ((@ (gi util) push-duplicate-handler!) 'shrug-equals)
           (unless (false-if-exception ((@ (gi) use-typelibs) lib ...))
             (exit EXIT_SKIPPED)))))))

(define (short-vector->list x)
  (cond
   ((= (sizeof short) 2)
    (s16vector->list x))
   ((= (sizeof short) 4)
    (s32vector->list x))
   (else
    (error "unknown short size"))))

(define (list->short-vector x)
  (cond
   ((= (sizeof short) 2)
    (list->s16vector x))
   ((= (sizeof short) 4)
    (list->s32vector x))
   (else
    (error "unknown short size"))))

(define (int-vector->list x)
  (cond
   ((= (sizeof int) 4)
    (s32vector->list x))
   ((= (sizeof int) 8)
    (s64vector->list x))
   (else
    (error "unknown int size"))))

(define (list->int-vector x)
  (cond
   ((= (sizeof int) 4)
    (list->s32vector x))
   ((= (sizeof int) 8)
    (list->s64vector x))
   (else
    (error "unknown int size"))))

(define (long-vector->list x)
  (cond
   ((= (sizeof long) 4)
    (s32vector->list x))
   ((= (sizeof long) 8)
    (s64vector->list x))
   (else
    (error "unknown long int size"))))

(define (list->long-vector x)
  (cond
   ((= (sizeof long) 4)
    (list->s32vector x))
   ((= (sizeof long) 8)
    (list->s64vector x))
   (else
    (error "unknown long int size"))))
