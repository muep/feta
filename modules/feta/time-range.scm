;; time-range.scm
;;
;; Time range data type. Used quite carelessly for
;; representing both times and time+accuracy pairs, too. And
;; sessions are also made by just adding stuff to the time
;; range data type.
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
;;     * Neither the name of the <organization> nor the names of its
;;       contributors may be used to endorse or promote products
;;       derived from this software without specific prior written
;;       permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
;; <COPYRIGHT HOLDER> BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

(define-module (feta time-range)
  :export (;; Functions
           make-time-range
           time-range?
           time-range-complete?
           time-range-duration
           time-range-end
           time-range-mid
           time-range-next
           time-range-prev
           time-range-start)
  :use-module (ice-9 optargs)
  :use-module (srfi srfi-19)
  :use-module (feta nih))

;; Convert a time given as an unix time stamp number into
;; the srfi-19 time record.
(define timize
  (lambda (ton)
    (cond ((eq? ton #f) #f)
          ((time? ton) ton)
          ((number? ton) (make-time 'time-utc 0 ton))
          (#t #f))))

(define (earlier? t0 t1)
  (if (eq? t1 #f)
      ;; Everything is earlier than #f
      #t
      ;; Otherwise defer to time<?
      (time<? t0 t1)))


(define (make-time-range start . end)
  (list
   (cons 'start (timize start))
   (cons 'end   (if (null? end) #f
                    (timize (car end))))))

(define (time-range? sth)
  (if (and
       (list? sth)
       (assoc 'start sth)
       (assoc 'end sth)) #t #f))

(define (time-range-complete? tr)
  (if (and (time-range? tr)
           (time-range-start tr)
           (time-range-end tr)) #t #f))

(define (time-range-duration tr)
  (let* ((start (time-range-start tr))
         (end   (time-range-end tr)))
    (if (not (and start end)) #f
        (time-difference end start))))

(define (time-range-end tr)
  (aget 'end tr))

(define (time-range-mid tr)
  (if (time-range-complete? tr)
      (add-duration
       (time-range-start tr)
       (make-time 'time-duration 0
                  (round ;; We really want an integer second
                   (/ (time-second (time-range-duration tr))
                      2))))
      #f))

(define (time-range-next tr)
  (if (time-range-complete? tr)
    (make-time-range (time-range-end tr)
                     (add-duration (time-range-end tr)
                                   (time-range-duration tr)))
    #f))

(define (time-range-prev tr)
  (if (time-range-complete? tr)
    (make-time-range (subtract-duration (time-range-start tr)
                                        (time-range-duration tr))
                     (time-range-start tr))
    #f))

(define (time-range-start tr)
  (aget 'start tr))
