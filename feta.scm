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

(define all
  (lambda (ok? l)
    (cond ((null? l) #t)
          ((not (ok? (car l))) #f)
          (#t (all ok? (cdr l))))))

;; A wrapper for reducing boilerplate in accessing alist
;; members
(define get
  (lambda (sym alist)
    (let ((tmp (assoc sym alist)))
      (if (eq? tmp #f) #f (cdr tmp)))))

;; Date conversion code
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

(define day-seconds 86400)
(define day-duration (make-time 'time-duration 0 day-seconds))
(define week-seconds (* 7 86400))
(define week-duration (make-time 'time-duration 0 week-seconds))

(define time->day-start-time
  (lambda (t)
    (let ((d (time-utc->date t)))
      (date->time-utc
       (make-date 0 0 0 0 ;; nanos secs mins hours
                  (date-day d)
                  (date-month d)
                  (date-year d)
                  (date-zone-offset d))))))

(define time->day-end-time
  (lambda (t)
    (add-duration (time->day-start-time t) day-duration)))

(define time->week-start-time
  (lambda (t)
    (let* ((monday-offset (modulo
                           (- (date-week-day (time-utc->date t)) 1)
                           7))
           (day-start (time->day-start-time t)))
      (subtract-duration
       day-start
       (make-time 'time-duration 0 (* monday-offset day-seconds))))))

(define time->week-end-time
  (lambda (t)
    (add-duration (time->week-start-time t) week-duration)))

;; Warn function that does nothing
(define warn
  (lambda (msg) #f))

(define simple-time-matcher
  (lambda (format accuracy)
    (lambda (str)
      (catch
       #t
       (lambda ()
         (cons (date->time-utc (string->date str format))
               accuracy))
       (lambda _ #f)))))

(define last-occurrence-of-clock-time
  (lambda (hours minutes seconds)
    (if (or (>= hours 24) (>= minutes 60) (>= seconds 60))
        (throw 'time-semantics)
        (let* ((d (current-date))
               (t (current-time 'time-utc))
               (d2 (make-date
                    (date-nanosecond d)
                    seconds minutes hours
                    (date-day d)
                    (date-month d)
                    (date-year d)
                    (date-zone-offset d)))
               (t2 (date->time-utc d2)))
          (if (time>=? t2 t)
              ;; Got a time in the future, subtract one day
              (subtract-duration t2 day-duration)
              t2)))))

;; This is a bit sucky but it tries to handle a lot of cases.
(define clock-time-matcher
  (lambda (str)
    (catch
     #t
     (lambda ()
       (let* ((pieces (string-split str #\:))
              (hours (string->number (car pieces)))
              (minutes (if (< (length pieces) 2)
                           0
                           (string->number (cadr pieces))))
              (seconds (if (< (length pieces) 3)
                           0
                           (string->number (caddr pieces))))
              (got-extra (< 3 (length pieces))))

         (if (and (not got-extra)
                  (all number? (list hours minutes seconds))
                  (< hours 24)
                  (< minutes 60)
                  (< seconds 60))
             (cons (last-occurrence-of-clock-time hours minutes seconds)
                   (case (length pieces)
                     ((1) 3600)
                     ((2) 60)
                     ((3) 1)
                     (else #f))))))
     (lambda _ #f))))

(define time+accuracy->time-range
  (lambda (t+a)
    (time-range-new
     (car t+a)
     (add-duration
      (car t+a)
      (make-time 'time-duration
                 (time-nanosecond (car t+a))
                 (+ (time-second (car t+a))
                    (cdr t+a)))))))

;; Define the list of matchers we will use for user-supplied
;; times. User-supplied times will be matched against these
(define time-matchers
  (list
   (simple-time-matcher "~Y-~m-~dT~H:~M:~S" 1)
   (simple-time-matcher "~Y-~m-~dT~H:~M" 60)
   (simple-time-matcher "~Y-~m-~dT~H" 3600)
   (simple-time-matcher "~Y-~m-~d" 86400)
   clock-time-matcher))

(define match-timestr
  (lambda (str matchers)
    (if (null? matchers)
        #f
        (let ((tpair ((car matchers) str)))
          (if (eq? #f tpair)
              (match-timestr str (cdr matchers))
              tpair)))))

(define fallbacky-string->time
  (lambda (str matchers)
    (let ((tp (match-timestr str matchers)))
      (if (eq? tp #f)
          #f
          (car tp)))))

;; Just a convenience wrapper
(define permissive-string->time
  (lambda (str)
    (fallbacky-string->time str time-matchers)))

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

(define session-time-range
  (lambda (session)
    (time-range-new
     (get 'start-time session)
     (time-or-now (get 'end-time session)))))

(define session-in-range?
  (lambda (session range)
    (time-range-contains?
     range
     (session-time-range session))))

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

(define session-thisweek?
  (lambda (session)
    (let* ((cdate (current-date))
           (today-start (date->time-utc
                         (make-date 0 0 0 0
                                    (date-day cdate)
                                    (date-month cdate)
                                    (date-year cdate)
                                    (date-zone-offset cdate))))
           (thisweek-start
            (make-time 'time-utc
                       (time-nanosecond today-start)
                       (- (time-second today-start)
                          (* (modulo (+ (date-week-day cdate) -1) 7)
                             86400))))
           (thisweek-end
            (make-time 'time-utc
                       (time-nanosecond thisweek-start)
                       (+ (time-second today-start)
                          (* 7 86400)))))
      (and (time<=? thisweek-start (get 'start-time session))
           (time>=? thisweek-end (time-or-now
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
             (cons "thisweek" session-thisweek?)
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
