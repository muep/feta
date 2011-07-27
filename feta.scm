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

(use-modules (srfi srfi-1))
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

;; A wrapper for reducing boilerplate in accessing alist
;; members
(define get
  (lambda (sym alist)
    (let ((tmp (assoc sym alist)))
      (if (eq? tmp #f) #f (cdr tmp)))))

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

(define time-or-now
  (lambda (t)
    (if (time? t) t (current-time 'time-utc))))

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
    (list
     (cons 'start-time time)
     (cons 'end-time #f)
     (cons 'description description))))

;; Return a function that closes sessions
(define session-closer
  (lambda (end-time)
    (lambda (session)
      (if (get 'end-time session)
          session
          (assoc-set! session 'end-time end-time)))))

(define string->session
  (lambda (line)
    (let ((tokens (string-split line #\;)))
      (list
       (cons 'start-time (db-string->time (car tokens)))
       (cons 'end-time (db-string->time (cadr tokens)))
       (cons 'description (string-join (cddr tokens) ""))))))

(define session<?
  (lambda (s0 s1)
    (time<? (get 'start-time s0)
            (get 'start-time s1))))

(define session->string
  (lambda (session)
    (let ((start-time (get 'start-time session))
          (end-time (get 'end-time session))
          (description (get 'description session)))
      (string-append
       (number->string (time-second start-time))
       ";"
       (if end-time
           (number->string (time-second end-time)) "")
       ";"
       description
       ";\n"))))

;; A structure that defines a slice of time
(define time-range-new
  (lambda (start end)
    (list
     (cons 'start start)
     (cons 'end end))))

(define time-range-start (lambda (range) (get 'start range)))
(define time-range-end (lambda (range) (get 'end range)))
(define time-range-length
  (lambda (range)
    (if (and (time? (get 'start range))
             (time? (get 'end range)))
        (time-difference (get 'end range) (get 'start range))
        #f)))

(define time-range-overlaps?
  (lambda (r0_ r1_)
    (let* ((should-swap (time<? (get 'start r1) (get 'start r0)))
           ;; Have known ordering for input ranges
           (r0 (if should-swap r1_ r0_))
           (r1 (if should-swap r0_ r1_)))

      (cond ((or (eq? (time-range-length r0) #f)
                 (eq? (time-range-length r1) #f))
             ;; Ranges are not completely defined
             #f)

            ((time<? (get 'end r0) (get 'start r1))
             ;; First time ends before latter one starts
             #f)

            (#t #t)))))

(define time-range-contains?
  (lambda (r0_ r1_)
    (let* ((should-swap (time<? (get 'start r1) (get 'start r0)))
           ;; Have known ordering for input ranges
           (r0 (if should-swap r1_ r0_))
           (r1 (if should-swap r0_ r1_)))

      (cond ((not (time-range-overlaps? r0 r1))
             #f)

            ((time<? (get 'end r0) (get 'end r1))
             ;; First range ends before the second range
             #f)

            (#t #t)))))

;; Like map but for lines in port
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

(define default-db-location (string-append (getenv "HOME") "/.ttdb"))
(define read-db
  (lambda (filename)
    (catch
     #t
     (lambda ()
       (sort
        (parse-db-port (open-file filename "r"))
        session<?))
     (lambda _ '()))))

(define write-db
  (lambda (db out)
    (for-each
     (lambda (session)
       (display (session->string session) out))
     db)))

(define session->userline
  (lambda (session)
    (let* ((from  (get 'start-time session))
           (to    (get 'end-time session))
           (descr (get 'description session)))
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
          (time-difference (time-or-now to)
                           from)))
        "\n")
       ""))))

(define descriptions
  (lambda (db)
    (uniquify
     (map
      (lambda (session)
        (get 'description session)) db))))

(define session<?
  (lambda (s0 s1)
    (time<? (get 'start-time s0)
            (get 'start-time s1))))

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


(define session-today?
  (lambda (session)
    (let* ((cdate (current-date))
           (today-start (date->time-utc
                         (make-date 0 0 0 0
                                    (date-day cdate)
                                    (date-month cdate)
                                    (date-year cdate)
                                    (date-zone-offset cdate))))
           (today-end (make-time 'time-utc
                                 (time-nanosecond today-start)
                                 (+ (time-second today-start) 86400))))
      (and (time<=? today-start (get 'start-time session))
           (time>=? today-end (time-or-now
                               (get 'end-time session)))))))

(define session-all
  (lambda (session)
    #t))

;; Actions:
;;   default
;;   info
;;   start
;;   end
;;

(define eta-like-main
  (lambda (argv)
    (let* ((helpmsg
            (string-append
             "Usage: "
             (car argv)
             " <options>\n"))

           (option-spec
            '((version
               (value #f))
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
               (single-char #\t))

              (file
               (value #t)
               (single-char #\f))

              (filter-time
               (value #t))

              ))

           (time-filters
            (list
             (cons "today" session-today?)
             (cons "always" session-all)))


           (options
            (getopt-long argv option-spec))

           (want-help (option-ref options 'help #f))
           (want-start  (option-ref options 'start #f))
           (want-end (option-ref options 'end #f))

           (time-filter-name
            (option-ref options 'filter-time "always"))

           (requested-time
            (let ((timearg (option-ref options 'time #f)))
              (if timearg
                  (permissive-string->time timearg)
                  (current-time 'time-utc))))

           (db-location
            (option-ref options 'file default-db-location))

           (db (read-db db-location))

           (requested-description
            (option-ref options 'description
                        (if (null? db)
                            "Default project"
                            (get 'description (last db)))))

           )

      (cond
       (want-help
        (begin
          (display helpmsg)
          (exit 0)))

       (want-start
        (let ((new-db (sort
                       (append
                        ;; Old database with its sessions closed
                        (map (session-closer requested-time) db)
                        ;; And an element with the added session
                        (list (session-new
                               requested-time
                               requested-description)))
                       session<?)))
          (write-db new-db (open-file db-location "w"))))


       (want-end
        (let ((new-db (sort
                       ;; Old database with its sessions closed
                       (map (session-closer requested-time) db)
                       session<?)))
          (write-db new-db (open-file db-location "w"))))

       (#t
        (let* ((tfilter (get time-filter-name time-filters))
               (interesting-sessions (filter tfilter db))
               (total-seconds
                (fold (lambda (cur sum)
                        (+ sum (- (time-second (time-or-now
                                                (get 'end-time cur)))
                                  (time-second (get 'start-time cur)))))
                      0
                      interesting-sessions)))
          (display-sessionlist interesting-sessions)
          (display "total: ")
          (display (duration->string total-seconds))
          (newline)
          ))))))


(eta-like-main (command-line))
