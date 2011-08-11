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

       (lambda ()
         "time-range-overlaps?"
         (let ((range0 (time-range-new
                        (make-time 'time-utc 0 1300000000)
                        (make-time 'time-utc 0 1300200000)))
               (range1 (time-range-new
                        (make-time 'time-utc 0 1300100000)
                        (make-time 'time-utc 0 1300300000)))
               (range2 (time-range-new
                        (make-time 'time-utc 0 1300250000)
                        (make-time 'time-utc 0 1300400000))))
           (cond
            ((time-range-overlaps? range0 range2)
             (throw 'FAIL "Ranges 0 and 2 should not overlap"))
            ((not (time-range-overlaps? range0 range1))
             (throw 'FAIL "Ranges 0 and 1 should overlap"))
            ((not (time-range-overlaps? range2 range1))
             (throw 'FAIL "Ranges 2 and 1 should overlap"))
            ((time-range-overlaps? range2 range0)
             (throw 'FAIL "Ranges 2 and 0 should not overlap"))
            (#f 'OK))))

       (lambda ()
         "session-in-range? accepts something"
         (let ((big-range (time-range-new
                           (make-time 'time-utc 0 1300000000)
                           (make-time 'time-utc 0 1310000000)))
               (small-session
                (list
                 (cons 'start-time (make-time 'time-utc 0 1301000000))
                 (cons 'end-time (make-time 'time-utc 0 1302000000))
                 (cons 'description "Working hard"))))
           (if (not (session-in-range? small-session big-range))
               (throw 'FAIL
                      (string-append
                       "Session\n"
                       (session->userline small-session)
                       "was not in big range")))))

       (lambda ()
         (let ((aw2-sessions
                (filter (lambda (session)
                          (session-in-range? session augw2-range))
                        sample-db)))
           (cond ((not (equal? (length aw2-sessions) 1))
                  (throw 'FAIL
                         (string-append
                          "Should have only one session in august w2, "
                          "instead got "
                          (number->string (length aw2-sessions)))))
                 ((not (equal? (get 'description
                                    (car aw2-sessions))
                               "feta4"))
                  (throw 'FAIL "Did not get the feta4 session"))
                 (#t 'OK))))

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
