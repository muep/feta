;; time-range.scm
;;
;; Time range data type

(define-module (feta time-range)
  :export (;; Functions
           make-time-range
           time-range?
           time-range-complete?
           time-range-duration
           time-range-end
           time-range-mid
           time-range-next
           time-range-prev
           time-range-start)
  :use-module (ice-9 optargs)
  :use-module (srfi srfi-19)
  :use-module (feta nih))

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
  (if (and
       (list? sth)
       (assoc 'start sth)
       (assoc 'end sth)) #t #f))

(define (time-range-complete? tr)
  (if (and (time-range? tr)
           (time-range-start tr)
           (time-range-end tr)) #t #f))

(define (time-range-duration tr)
  (let* ((start (time-range-start tr))
         (end   (time-range-end tr)))
    (if (not (and start end)) #f
        (time-difference end start))))

(define (time-range-end tr)
  (aget 'end tr))

(define (time-range-mid tr)
  (if (time-range-complete? tr)
      (add-duration
       (time-range-start tr)
       (make-time 'time-duration 0
                  (round ;; We really want an integer second
                   (/ (time-second (time-range-duration tr))
                      2))))
      #f))

(define (time-range-next tr)
  (if (time-range-complete? tr)
    (make-time-range (time-range-end tr)
                     (add-duration (time-range-end tr)
                                   (time-range-duration tr)))
    #f))

(define (time-range-prev tr)
  (if (time-range-complete? tr)
    (make-time-range (subtract-duration (time-range-start tr)
                                        (time-range-duration tr))
                     (time-range-start tr))
    #f))

(define (time-range-start tr)
  (aget 'start tr))
