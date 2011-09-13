;; localtime.scm
;;
;; Some handy local time computation functions

(define-module (feta localtime)
  :export (;; Mappings from times to ranges
           day-of
           month-of
           week-of
           year-of

           ;; Mappings from times to times
           end-of-day
           end-of-month
           end-of-week
           end-of-year
           start-of-day
           start-of-month
           start-of-week
           start-of-year

           ;; Other useful items
           now
           month-with-number
           week-with-number)

  :use-module (srfi srfi-19)
  :use-module (feta time-range))


(define day-seconds 86400)
(define day-duration (make-time 'time-duration 0 day-seconds))
(define week-seconds (* 7 86400))
(define week-duration (make-time 'time-duration 0 week-seconds))

(define localtime-offset-at
  (lambda (t)
    (let ((d (time-utc->date t)))
      (date-zone-offset d))))

(define date->time-local
  (lambda (d)
    (let ((attempt (date->time-utc d))))))

(define day-of
  (lambda (t)
    (make-time-range (start-of-day t) (end-of-day t))))

(define month-of
  (lambda (t)
    (make-time-range (start-of-month t) (end-of-month t))))

(define week-of
  (lambda (t)
    (make-time-range (start-of-week t) (end-of-week t))))

(define year-of
  (lambda (t)
    (make-time-range (start-of-year t) (end-of-year t))))

(define end-of-day
  (lambda (t)
    (add-duration (start-of-day t) day-duration)))

(define end-of-month
  (lambda (t)
    (let* ((month-start (start-of-month t))
           ;; Get to a bit past the next month's start
           (next-month-beginish
            (add-duration month-start
                          (make-time 'time-duration
                                     0
                                     (* 32 day-seconds)))))
      (start-of-month next-month-beginish))))

(define end-of-week
  (lambda (t)
    (add-duration (start-of-week t) week-duration)))

(define end-of-year
  (lambda (t)
    (let* ((year-start (start-of-year t))
           ;; Get to a bit past the next year's start
           (next-year-beginish
            (add-duration year-start
                          (make-time 'time-duration
                                     0
                                     (* 367 day-seconds)))))
      (start-of-year next-year-beginish))))

(define start-of-day
  (lambda (t)
    (let ((d (time-utc->date t)))
      (date->time-utc
       (make-date 0 0 0 0 ;; nanos secs mins hours
                  (date-day d)
                  (date-month d)
                  (date-year d)
                  (date-zone-offset d))))))

(define start-of-month
  (lambda (t)
    (let ((d (time-utc->date t)))
      (date->time-utc
       (make-date 0 0 0 0 ;; nanos secs mins hours
                  1 ;; day
                  (date-month d)
                  (date-year d)
                  (date-zone-offset d))))))

(define start-of-week
  (lambda (t)
    (let* ((monday-offset (modulo
                           (- (date-week-day (time-utc->date t)) 1)
                           7))
           (day-start (start-of-day t)))
      (subtract-duration
       day-start
       (make-time 'time-duration 0 (* monday-offset day-seconds))))))

(define start-of-year
  (lambda (t)
    (let ((d (time-utc->date t)))
      (date->time-utc
       (make-date 0 0 0 0 ;; nanos secs mins hours
                  1 ;; day
                  1 ;; month
                  (date-year d)
                  (date-zone-offset d))))))


(define now (lambda () (current-time 'time-utc)))