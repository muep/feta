#!/usr/bin/guile -s
!#

(load "feta.scm")

(use-modules (srfi srfi-19))

;; Time range that spans July
(define july-range
  (time-range-new
   (make-time 'time-utc 0 1312952400)
   (make-time 'time-utc 0 1312146000)))

(define august-range
  (time-range-new
   (make-time 'time-utc 0 1312146000)
   (make-time 'time-utc 0 1314824400)))

(define augw1-range
  (time-range-new
   (make-time 'time-utc 0 1312146000)
   (make-time 'time-utc 0 1312750800)))

(define augw2-range
  (time-range-new
   (make-time 'time-utc 0 1312750800)
   (make-time 'time-utc 0 1313355600)))


(define sample-db
  (list
   ;; Session on last week of July
   (list
    (cons 'start-time (make-time 'time-utc 0 1311829200))
    (cons 'end-time (make-time 'time-utc 0 1311840000))
    (cons 'description "feta1"))

   ;; A couple of sessions in first week of August
   (list
    (cons 'start-time (make-time 'time-utc 0 1312174800))
    (cons 'end-time (make-time 'time-utc 0 1312185600))
    (cons 'description "feta2"))
   (list
    (cons 'start-time (make-time 'time-utc 0 1312189200))
    (cons 'end-time (make-time 'time-utc 0 1312203600))
    (cons 'description "feta3"))

   ;; A session on second week of August
   (list
    (cons 'start-time (make-time 'time-utc 0 1312952400))
    (cons 'end-time (make-time 'time-utc 0 1312963200))
    (cons 'description "feta4"))))

(let
    (
     ;; List of test thunks
     (tests
      (list
       (lambda ()
         "Just dump the session list to verify it
          has something sensible in it"
         (display-sessionlist sample-db))
       )
      )
     ;; Store information about failures here
     (fail-messages '()))

  (for-each
   (lambda (test)
     (catch
      ;; Catch all...
      #t
      ;; in the current test...
      test
      ;; and add exceptions thrown to the
      ;; fail-messages list
      (lambda _
        (set! fail-messages
              (cons _ fail-messages)))))
   tests)

  (if (equal? (length fail-messages) 0)
      (begin
        (display "All tests passed! great!")
        (newline))
      (for-each
       (lambda (fail)
         (display "FAIL: ")
         (display fail)
         (newline))
       fail-messages)))
