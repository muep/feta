;; xml.scm
;;
;; Generate an xml data from a set of sessions
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

(define-module (feta xml)
  :export (sessions->sxml
           sessions->xhtml
           session->xhtmlrow session)
  :use-module (srfi srfi-19)
  :use-module (feta nih)
  :use-module (feta session))

(define (sessions->sxml sessions)
  '(sessions))

(define xhtml-title "Feta session report")

(define (timify t)
  (cond ((time? t) t)
        ((number? t) (make-time 'time-utc 0 t))
        (#t #f)))

(define (time->string t)
  (date->string (time-utc->date (timify t))
                "~Y-~m-~d ~H:~M"))

(define (time->clockstr t)
  (date->string (time-utc->date (timify t))
                "~H:~M"))

(define (session-starts s)
  (time-second (session-start s)))

(define (session-ends s)
  (time-second (session-end s)))

(define (sessions->xhtml sessions)
  `(html
    (head
     (title ,xhtml-title))
    (body
     (h1 ,xhtml-title)
     (p "From " ,(time->string
                  (apply min (map session-starts sessions)))
        (br)
        " to " ,(time->string
                 (apply max (map session-ends sessions))) ".")
     ,(sessions->xhtmltable sessions))))

(define (sessions->xhtmltable sessions)
  `(table (@ (border 1))
          (tr (th "Start") (th "End")
              (th "Duration") (th "Description"))
          ,(map session->xhtmlrow sessions)))

(define (session->xhtmlrow session)
  (let* ((starts (time-second (session-start session)))
         (ends (time-second (session-end session)))
         (durstr (duration->string (- ends starts))))
  `(tr (td ,(time->string starts))
       (td ,(time->clockstr ends))
       (td ,durstr)
       (td ,(session-description session)))))
