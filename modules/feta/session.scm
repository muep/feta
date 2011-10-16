;; session.scm
;;
;; Sessions managed by feta

(define-module (feta session)
  :export (make-session
           make-session-from
           session?
           session-description
           session-end
           session-finished?
           session-start
           session-time-range)
  :use-module (ice-9 optargs)
  :use-module (feta nih)
  :use-module (feta time-range))

;; Here we quite rudely depend on our time-range
;; implementation detail of alistness.
(define (make-session descr tr)
  (acons 'description descr tr))

(define* (make-session-from session
                            #:key end description)
  (append
   (if end
       (make-time-range (session-start session)
                        end)
       '())
   (if description
       (list (cons 'description 'description)) '())
   session))

(define (session? s)
  (if (and (time-range? s)
           (assoc 'description s)) #t #f))

(define (session-description s)
  (aget 'description s))

(define session-start time-range-start)
(define session-end time-range-end)
(define session-finished? time-range-complete?)
(define (session-time-range s-tr) s-tr)
