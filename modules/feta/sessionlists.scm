;; sessionlists.scm
;;
;; Utilities for managing sets of sessions
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

(define-module (feta sessionlists)
  :export (sl-crop
           sl-kill
           sl-merge)
  :use-module (srfi srfi-19)
  :use-module (feta session)
  :use-module (feta time-range))

;; Picks only sessions which fit inside the to-range. Also
;; clips individual sessions to fit into the desired range
;; while doing so.
(define (sl-crop to-range sessions)
  (throw 'i-am-not-defined))

;; Evaluates to a list of sessions with no session
;; intersecting from-range.
(define (sl-kill from-range sessions)
  (throw 'i-am-not-defined))

(define (sl-merge . sessionlistlist)
  "Merges a number of session lists into one session list"
  ;; Ensure that all sessionlists are in chronological order,
  ;; and process further in sl-merge-fn
  (sl-merge-fn '() (map (lambda (slist)
                          (sort slist session-before?))
                        sessionlistlist)))

(define (session-before? s0 s1)
  (time<? (session-start s0) (session-start s1)))

(define (sl-merge-fn prevs slistlist)
  (let* (;; Ignore empty lists
         (non-emptys (filter (lambda (slist) (not (null? slist)))
                             slistlist))
         ;; We want to take the earliest available session,
         ;; so we sort a bit again
         (sorteds (sort non-emptys (lambda (slist0 slist1)
                                    (session-before? (car slist0)
                                                     (car slist1))))))
    (if (null? sorteds)
        ;; We ran out of data. Remove prevs after
        ;; pruning overlaps
        (without-overlaps (reverse prevs))

        ;; Still stuff to do, so let's do it again
        (sl-merge-fn
         ;; First item from first list is pushed to
         ;; beginning of prevs.
         (cons (car (car sorteds)) prevs)
         (cons
          ;; Rest of first list
          (cdr (car sorteds))
          ;; And the other lists
          (cdr sorteds))))))

(define (without-overlaps slist)
  (without-overlaps-fn '() slist))

(define (without-overlaps-fn prevs slist)
  (cond
   ((null? slist) (reverse prevs)) ;; Had no input data.
   ((null? (cdr slist)) (without-overlaps-fn
                         (cons (car slist) prevs)
                         (cdr slist)))
   (#t ;; We have at least two items of input
    (let* ((prev (car slist))
           (next (cadr slist))

           (prevstart (time-second (session-start prev)))
           (prevend   (time-second (session-end   prev)))
           (nextstart (time-second (session-start next)))
           (nextend   (time-second (session-end   next)))

           (can-combine (equal? (session-description prev)
                                (session-description next)))
           (got-overlap (>= prevend nextstart)))

      (cond ((and got-overlap can-combine)
             (without-overlaps-fn
              ;; Can not accept anything yet
              prevs

              (cons
               ;; New session from the two first sessions
               (make-session (session-description (car slist))
                             (make-time-range prevstart nextend))
               ;; And the rest of the input list
               (cddr slist))))
            ((not got-overlap)
             (without-overlaps-fn
              ;; No overlap, so accept first
              (cons prev prevs)
              ;; And keep processing the rest
              (cdr slist)))
            (#t (throw 'can-not-combine
                       "Different session descriptions")))))))
