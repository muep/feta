#!/usr/bin/guile -s
!#
;; feta.scm
;;
;; feta - eta in functional style
;;
;; Toy project for a scheme program that imitates and strives for
;; compatibility with embelin-time-assistant from
;; https://gitorious.org/embelin-time-assistant/ Written most
;; importantly just for learning scheme.
;;
;; Copyright (c) 2011, Joonas Sarajärvi <muepsj@gmail.com>
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;     * Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;     * Neither the name of the <organization> nor the names of its
;;       contributors may be used to endorse or promote products
;;       derived from this software without specific prior written
;;       permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
;; <COPYRIGHT HOLDER> BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

(use-modules (ice-9 getopt-long))
(use-modules (ice-9 rdelim))

(use-modules (srfi srfi-19))

;; Generic list utilities (TODO: find out if this already
;; exists somewhere)
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

(define last
  (lambda (l)
    (car (last-pair l))))

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
  "~Y-~m-~dT~H:~M")
(define user-nonexistent-time-format
  "   until now    ")

(define time->string
  (lambda (time)
    (if (time? time)
        (date->string (time-utc->date time) user-time-format)
        user-nonexistent-time-format)))

(define duration->string
  (lambda (seconds)
    (let* ((hours (floor (/ seconds 3600)))
           (remaining (modulo seconds 3600))
           (mins (floor (/ remaining 60))))
      (string-append (number->string hours) ":"
                     (number->string mins)))))

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
                 (fallbacky-string->time str (cdr formats)))))))

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

(define session-new
  (lambda (time description)
    (acons 'start-time time
           (acons 'end-time #f
                  (acons 'description description '())))))

(define session-end-time
  (lambda (session)
    (cdr (assoc 'end-time session))))

;; Return a function that closes sessions
(define session-closer
  (lambda (end-time)
    (lambda (session)
      (if (session-end-time session)
          session
          (assoc-set! session 'end-time end-time)))))

(define string->session
  (lambda (line)
    (let* ((tokens (string-split line #\;))
           (start-time (db-string->time (car tokens)))
           (end-time (db-string->time (car (cdr tokens))))
           (description (string-join (cddr tokens) "")))

      (list
       (cons 'start-time start-time)
       (cons 'end-time end-time)
       (cons 'description description)))))

(define session<?
  (lambda (s0 s1)
    (time<? (cdr (assoc 'start-time s0))
            (cdr (assoc 'start-time s1)))))

(define session->string
  (lambda (session)
    (let ((start-time (cdr (assoc 'start-time session)))
          (end-time (cdr (assoc 'end-time session)))
          (description (cdr (assoc 'description session))))
      (string-append
       (number->string (time-second start-time))
       ";"
       (if end-time
           (number->string (time-second end-time)) "")
       ";"
       description
       ";\n"))))

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
    (read-lines in-port string->session)))

(define db-location (string-append (getenv "HOME") "/.ttdb"))
(define read-db
  (lambda ()
    (sort
     (parse-db-port (open-file db-location "r"))
     session<?)))

(define write-db
  (lambda (db out)
    (for-each
     (lambda (session)
       (display (session->string session) out))
     db)))

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
        (duration->string
         (time-second
          (time-difference (if (time? to)
                               to
                               (current-time 'time-utc))
                           from)))
        "\n")
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


;; Actions:
;;   default
;;   info
;;   start
;;   end
;;

(define argv (command-line))

(define helpmsg
  (string-append
   "Usage: "
   (car argv)
   " <options>\n"))

(define option-spec
  '((version (value #f))
    (help
     (value #f)
     (single-char #\h))

    (start
     (value #f)
     (single-char #\s))

    (end
     (value #f)
     (single-char #\e))

    (description
     (value #t)
     (single-char #\d))

    (time
     (value #t)
     (single-char #\t)
     )))


;; Main program starts here.
(define options
  (getopt-long (command-line) option-spec))

(if (option-ref options 'help #f)
    (begin
      (display helpmsg)
      (exit 0)))

(define requested-time
  (let ((timearg (option-ref options 'time #f)))
    (if timearg
        (permissive-string->time timearg)
        (current-time 'time-utc))))

(define requested-description
  (option-ref options 'description
              (if (null? db)
                  "Default project"
                  (cdr (assoc 'description (car (last-pair db)))))))

(define want-start  (option-ref options 'start #f))
(define want-end (option-ref options 'end #f))

(if (and want-start want-end)
    (begin
      (display "Can not both start and end\n")
      (display helpmsg)
      (exit 1)))

(cond
 (want-start
  (let ((new-db (sort
                 (append
                  ;; Old database with itse sessions closed
                  (map (session-closer requested-time) db)
                  ;; And an element with the added session
                  (list (session-new requested-time requested-description)))
                 session<?)))
    (write-db new-db (open-file db-location "w"))))
 (want-end
  (let ((new-db (sort
                 ;; Old database with itse sessions closed
                 (map (session-closer requested-time) db)
                 session<?)))
    (write-db new-db (open-file db-location "w"))))
 (#t (display-sessionlist db)))
