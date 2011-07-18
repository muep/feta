#!/usr/bin/guile -s
!#

(use-modules (ice-9 rdelim))

(define parse-db-line
  (lambda (line)
    (let* ((tokens (string-split line #\;))
           (start-time (string->number (car tokens)))
           (end-time (string->number (car (cdr tokens))))
           (description (string-join (cddr tokens))))

      (acons 'start-time start-time
             (acons 'end-time end-time
                    (acons 'description description '()))))))

(define read-lines-with
  (lambda (port f prevs)
    (let ((line (read-line port)))
      (if (string? line)
          (cons (f line) (read-lines-with port f prevs))
          prevs))))

(define read-lines
  (lambda (port f)
    (read-lines-with port f '())))

(define parse-db-port
  (lambda (in-port)
    (read-lines in-port parse-db-line)))

(define db-location (string-append (getenv "HOME") "/.ttdb"))
(define open-db (lambda () (open-file db-location "r")))
