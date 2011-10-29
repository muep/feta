;; time-range-match.scm
;;
;; Matches user supplied strings into time ranges.
;;
;; Copyright (c) 2011, Joonas Sarajärvi <muep@iki.fi>
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;     * Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(define-module (feta time-range-match)
  :export (string->time-range)
  :use-module ((srfi srfi-19)

               ;; We select this stuff explicitly to avoid
               ;; a warning in Guile 1.8.
               :select (add-duration
                        make-time
                        date->time-utc
                        string->date
                        time-duration))
  :use-module (feta time-range)
  :use-module (feta localtime))

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

(define kw-matcher
  (lambda (keyword result)
    (lambda (str)
      (if (equal? str keyword)
          (result)
          (throw 'no-match)))))

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
   ;; This must be the first matcher. It tries to Split its
   ;; input in half at ".."  and uses the rest of the
   ;; matchers for both of the pieces.
   (list
    (lambda (str)
      (let ((posof.. (string-contains str "..")))
        (if posof..
            (make-time-range
             (match (substring str 0 posof..) (cdr matchers))
             (match (substring str (+ 2 posof..))   (cdr matchers)))
            (throw 'no-match)))))

   ;; First some trivial matchers
   (list
    (kw-matcher "today"
                (lambda ()
                  (day-of (now))))
    (kw-matcher "thisweek"
                (lambda ()
                  (week-of (now))))
    (kw-matcher "thismonth"
                (lambda ()
                  (month-of (now)))))

   ;; Then ones based on string->date
   (map (lambda (format)
          (make-simple-matcher (car format) (cdr format)))
        formats)))

(define (match str mcrs)
  (if (null? mcrs)
      (throw 'no-match)
      (catch 'no-match
             (lambda ()
               ((car mcrs) str))
             (lambda _ (match str (cdr mcrs))))))

(define (string->time-range str)
  (catch 'no-match
         (lambda ()
           (match str matchers))
         (lambda _ #f)))
