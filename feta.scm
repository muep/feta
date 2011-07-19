#!/usr/bin/guile -s
!#

;; Line-oriented port reading
(use-modules (ice-9 rdelim))

;; Time/date library.
(use-modules (srfi srfi-19))

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
           (start-time (make-time
                        'time-utc
                        0
                        (string->number (car tokens))))
           (end-time (make-time
                      'time-utc
                      0
                      (string->number (car (cdr tokens)))))
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
(define open-db (lambda () (open-file db-location "r")))

(define session->userline
  (lambda (session)
    (let* ((from (cdr (assoc 'start-time session)))
           (to (cdr (assoc 'end-time session))))
      (string-join
       (list
        "From "
        (time->string from)
        " to "
        (time->string to)
        " ("
        (number->string
         (time-second
          (time-difference from
                           (if (time? to)
                               to
                               (current-time 'time-utc)))))
        ") seconds\n")
       ""))))


(define display-sessionlist
  (lambda (sessions)
    (for-each display (map session->userline sessions))))


(display-sessionlist (parse-db-port (open-db)))
