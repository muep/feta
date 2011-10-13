;; session.scm
;;
;; Sessions managed by feta

(define-module (feta session)
  :export (make-session
           session?
           session-description
           session-time-range)
  :use-module (feta time-range))

(define s-type
  (make-record-type "session" '(description time-range)))

(define s-make
  (record-constructor s-type '(description time-range)))

(define s-descr
  (record-accessor s-type 'description))

(define s-tr
  (record-accessor s-type 'time-range))

(define make-session s-make)
(define session? (record-predicate s-type))
(define session-description s-descr)
(define session-time-range s-tr)