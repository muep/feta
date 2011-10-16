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
           time-range-next
           time-range-prev
           time-range-start)
  :use-module (ice-9 optargs)
  :use-module (srfi srfi-19))

;; Convert a time given as an unix time stamp number into
;; the srfi-19 time record.
(define timize
  (lambda (ton)
    (cond ((eq? ton #f) #f)
          ((time? ton) ton)
          ((number? ton) (make-time 'time-utc 0 ton))
          (#t #f))))

(define (earlier? t0 t1)
  (if (eq? t1 #f)
      ;; Everything is earlier than #f
      #t
      ;; Otherwise defer to time<?
      (time<? t0 t1)))


(define (make-time-range start . end)
  (list
   (cons 'start (timize start))
   (cons 'end   (if (null? end) #f
                    (timize (car end))))))

(define (time-range? sth)
  (if (and (assoc 'start sth) (assoc 'end sth)) #t #f))

(define (time-range-duration tr)
  (let* ((start (time-range-start tr))
         (end   (time-range-end tr)))
    (if (not (and start end)) #f
        (time-difference end start))))

(define (time-range-end tr)
  (let ((c (assoc 'end tr)))
    (if c (cdr c) #f)))

(define (time-range-mid tr)
  (add-duration
   (time-range-start tr)
   (make-time 'time-duration 0
              (round ;; We really want an integer second
               (/ (time-second (time-range-duration tr))
                  2)))))

(define time-range-next
  (lambda (tr)
    (make-time-range (time-range-end tr)
                     (add-duration (time-range-end tr)
                                   (time-range-duration tr)))))

(define time-range-prev
  (lambda (tr)
    (make-time-range (subtract-duration (time-range-start tr)
                                        (time-range-duration tr))
                     (time-range-start tr))))

(define (time-range-start tr)
  (let ((c (assoc 'start tr)))
    (if c (cdr c) #f)))
