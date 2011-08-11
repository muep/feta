#!/usr/bin/guile -s
!#

(load "feta.scm")


(let
    (
     ;; Store information about failures here
     (fail-messages '())

     ;; List of test thunks
     (tests
      (list
       (lambda ()
         (throw 'foo 'bar))
       (lambda ()
         #t))))

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
