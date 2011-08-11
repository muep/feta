#!/usr/bin/guile -s
!#

(load "feta.scm")

(use-modules (srfi srfi-19))

(define sample-db
  (list
   ;; Session on last week of July
   (list
    (cons 'start-time (make-time 'time-utc 0 1311829200))
    (cons 'end-time (make-time 'time-utc 0 1311840000))
    (cons 'description "feta"))

   ;; A couple of sessions in first week of August
   (list
    (cons 'start-time (make-time 'time-utc 0 1312174800))
    (cons 'end-time (make-time 'time-utc 0 1312185600))
    (cons 'description "feta"))
   (list
    (cons 'start-time (make-time 'time-utc 0 1312189200))
    (cons 'end-time (make-time 'time-utc 0 1312203600))
    (cons 'description "feta"))

   ;; A session on second week of August
   (list
    (cons 'start-time (make-time 'time-utc 0 1312952400))
    (cons 'end-time (make-time 'time-utc 0 1312963200))
    (cons 'description "feta"))))

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
