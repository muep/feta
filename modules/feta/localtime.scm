;; localtime.scm
;;
;; Some handy local time computation functions
;; This is actually quite annoying to get correct.

(define-module (feta localtime)
  :export (;; A very important missing time conversion
           local-date->time-utc

           ;; Mappings from times to ranges
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

(define offset-at
  (lambda (t)
    (let ((d (time-utc->date t)))
      (date-zone-offset d))))

;; Convert a date into a timestamp while ignoring the
;; timestamp and assuming that the date is in local
;; offset. This is tricky because the offset actually
;; depends on what the date is. And it is currently badly
;; implemented.
(define local-date->time-utc
  (lambda (d0)
    (let* (;; An offset "guess" from the original date.  In
           ;; many cases this is off from desired offset
           ;; only by 1 hour at maximum.
           (offset0 (date-zone-offset d0))

           ;; Let's just convert it blindly first. This may
           ;; be off by one hour in cases we cross the DST
           ;; switch time.
           (t0 (date->time-utc d0))

           ;; Convert it back to a date. This gets us the
           ;; UTC offset at the specified time. Now NOTE:
           ;; this was taken from a time that was possibly
           ;; one hour off!
           (d1 (time-utc->date t0))

           ;; And the from the nearly-correct time
           (offset1 (date-zone-offset d1))

           ;; Positive when our guess had drifted forwards
           (offset-error (- offset1 offset0)))
      ;; We fail badly in the corner case where the first guess throws
      ;; us to the wrong side of a DST transition.
      (subtract-duration t0 (make-time 'time-duration 0 offset-error)))))

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