;; time-range-match.scm
;;
;; Matches user supplied strings into time ranges

(define-module (feta time-range-match)
  :export (string->time-range)
  :use-module ((srfi srfi-19)
               :select (add-duration
                        make-time
                        date->time-utc
                        string->date
                        time-duration))
  :use-module (feta time-range))

(define make-simple-matcher
  (lambda (format accuracy)
    (lambda (str)
      (catch
       'misc-error
       (lambda ()
         (let ((startt (date->time-utc (string->date str format))))
           (make-time-range
            startt
            (add-duration startt
                          (make-time 'time-duration 0 accuracy)))))
       (lambda _ (throw 'no-match))))))

;; A set of fixed formats supported by
;; string->date which we can conveniently match
;; against
(define formats
  '(("~Y-~m-~dT~H:~M:~S" . 1)
    ("~Y-~m-~dT~H:~M"    . 60)
    ("~Y-~m-~dT~H"       . 3600)
    ("~Y-~m-~d"          . 86400)))

;; A list of all possible ways for us to match
(define matchers
  (append
   (map (lambda (format)
          (make-simple-matcher (car format) (cdr format)))
        formats)))

(define string->time-range
  (lambda (str)
    (letrec ((match (lambda (str mcrs)
                      (if (null? mcrs) #f
                          (catch 'no-match
                                 (lambda ()
                                   ((car mcrs) str))
                                 (lambda _ (match str (cdr mcrs))))))))
      (match str matchers))))
