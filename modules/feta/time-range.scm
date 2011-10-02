;; time-range.scm
;;
;; Time range data type

(define-module (feta time-range)
  :export (;; Functions
           make-time-range
           time-range?
           time-range-duration
           time-range-end
           time-range-mid
           time-range-start)
  :use-module ((srfi srfi-19)
               :select (add-duration
                        make-time
                        time?
                        time<?
                        time-difference
                        time-second
                        time-utc)))

(define tr-type
  (make-record-type "time-range" '(start length)))

(define tr-make
  (record-constructor tr-type '(start length)))

(define tr-start
  (record-accessor tr-type 'start))
(define tr-len
  (record-accessor tr-type 'length))

;; Convert a time given as an unix time stamp number into
;; the srfi-19 time record.
(define timize
  (lambda (ton)
    (cond ((time? ton) ton)
          ((number? ton) (make-time 'time-utc 0 ton))
          (#t #f))))

(define earliest
  (lambda (times)
    (if (equal? (length times) 1)
        (car times)
        (let ((t1 (car times))
              (t2 (cadr times))
              (rest (cddr times)))
          (earliest (cons
                     (if (time<? t1 t2) t1 t2)
                     rest))))))

(define normalise-start-end
  (lambda (start end)
    (let ((start-time (timize start))
          (end (timize end)))
      (list start end))))

(define make-time-range
  (lambda (start end)
    (let ((times (sort (list (timize start) (timize end)) time<?)))
      (tr-make (car times) (time-difference (cadr times) (car times))))))

(define time-range?
  (record-predicate tr-type))

(define time-range-duration
  tr-len)
(define time-range-end
  (lambda (tr) (add-duration (tr-start tr)
                             (tr-len tr))))
(define time-range-mid
  (lambda (tr) (add-duration
                (tr-start tr)
                (make-time 'time-duration 0 (round
                                             (/ (time-second (tr-len tr))
                                                2))))))
(define time-range-start
  tr-start)
