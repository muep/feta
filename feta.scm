#!/usr/bin/guile -s
!#
;; feta.scm
;;
;; feta - eta in functional style
;;
;; Toy project for a scheme program that imitates and
;; strives for compatibility with embelin-time-assistant
;; from https://gitorious.org/embelin-time-assistant/
;; Written most importantly just for learning scheme.
;;
;; Copyright (c) 2011, Joonas Sarajärvi <muepsj@gmail.com>
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the <organization> nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Line-oriented port reading
(use-modules (ice-9 rdelim))

;; Time/date library.
(use-modules (srfi srfi-19))

;; Generic list utilities (TODO: find out if this already exists somewhere)
;; TODO: get a high
(define remove-adjacents
  (lambda (list match? prev)
    (if (null? list)
        '()
        (let ((head (car list))
              (tail (cdr list)))
          (if (match? head prev)
              (remove-adjacents tail match? head)
              (cons head (remove-adjacents tail match? head)))))))

(define uniquify
  (lambda (list)
    (remove-adjacents (sort list string<?) equal? "")))

;; Date conversion code
(define full-date-format "~Y-~m-~dT~H:~M:~S")
(define date-formats
  (list full-date-format
        "~Y-~m-~dT~H:~M"
        "~Y-~m-~d"))
(define time-formats
  '("~H:~M:~S"
    "~H:~M"))

(define user-time-format
  "~Y-~m-~dT ~H:~M")
(define user-nonexistent-time-format
  "(time ain't defined)")

(define time->string
  (lambda (time)
    (if (time? time)
        (date->string (time-utc->date time) user-time-format)
        user-nonexistent-time-format)))

;; Warn function that does nothing
(define warn
  (lambda (msg) #f))

(define fallbacky-string->time
  (lambda (str formats)
    (if (null? formats)
        ;; We ran out of formats.
        #f
        ;; Try to parse with the first format in format list.
        (catch 'misc-error
               ;; The actual attepmt to convert is here.
               (lambda ()
                 (date->time-utc (string->date str (car formats))))
               ;; On failure, just keep trying with the rest of the
               ;; format list.
               (lambda _
                 (fallbacky-string->date str (cdr formats)))))))

;; Just a convenience wrapper
(define permissive-string->time
  (lambda (str)
    (fallbacky-string->time str date-formats)))

;; Also need a way to extract times from database format.
;; This is easier because we need not support so many forms of input.
(define db-string->time
  (lambda (str)
    (catch #t
           (lambda ()
             (make-time 'time-utc 0
                        (string->number str)))
           (lambda _ #f))))

(define parse-db-line
  (lambda (line)
    (let* ((tokens (string-split line #\;))
           (start-time (db-string->time (car tokens)))
           (end-time (db-string->time (car (cdr tokens))))
           (description (string-join (cddr tokens))))

      (acons 'start-time start-time
             (acons 'end-time end-time
                    (acons 'description description '()))))))

(define read-lines-with
  (lambda (port f prevs)
    (let ((line (read-line port)))
      (if (string? line)
          (catch #t
                 (lambda ()
                   (cons
                    (f line)
                    (read-lines-with port f prevs)))
                 (lambda _
                   (read-lines-with port f prevs)))
          prevs))))

(define read-lines
  (lambda (port f)
    (read-lines-with port f '())))

(define parse-db-port
  (lambda (in-port)
    (read-lines in-port parse-db-line)))

(define db-location (string-append (getenv "HOME") "/.ttdb"))
(define read-db (lambda () (parse-db-port (open-file db-location "r"))))

(define session->userline
  (lambda (session)
    (let* ((from  (cdr (assoc 'start-time session)))
           (to    (cdr (assoc 'end-time session)))
           (descr (cdr (assoc 'description session))))
      (string-join
       (list
        "From "
        (time->string from)
        " to "
        (time->string to)
        " on \""
        descr
        "\" for "
        (number->string
         (time-second
          (time-difference (if (time? to)
                               to
                               (current-time 'time-utc))
                           from)))
        " seconds\n")
       ""))))

(define db
  (catch #t
         (lambda ()
           (read-db))
         (lambda _ '())))

(define descriptions
  (lambda (db)
    (uniquify
     (map
      (lambda (session)
        (cdr (assoc 'description session))) db))))

(define session<?
  (lambda (s0 s1)
    (time<? (cdr (assoc 'start-time s0))
            (cdr (assoc 'start-time s1)))))

(define new-session
  (lambda (time description)
    (acons 'start-time time
           (acons 'end-time #f
                  (acons 'description description '())))))

;; Some simple display functions for
;; our simple cases.
(define display-sessionlist
  (lambda (sessions)
    (for-each display (map session->userline sessions))))

(define display-descriptions
  (lambda (db)
    (for-each
     (lambda (descr)
       (display descr)
       (newline))
     (descriptions db))))

;; Main program starts here.
(display-sessionlist db)
