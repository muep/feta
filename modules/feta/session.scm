;; session.scm
;;
;; Sessions as managed in feta.
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
