;; session.scm
;;
;; Sessions managed by feta

(define-module (feta session)
  :export (make-session
           session?
           session-description
           session-end
           session-start
           session-time-range)
  :use-module (feta nih)
  :use-module (feta time-range))

;; Here we quite rudely depend on our time-range
;; implementation detail of alistness.
(define (make-session descr tr)
  (acons 'description descr tr))

(define (session? s)
  (if (and (time-range? s)
           (assoc 'description s)) #t #f))

(define (session-description s)
  (aget 'description s))

(define session-start time-range-start)
(define session-end time-range-end)
(define (session-time-range s-tr) s-tr)
