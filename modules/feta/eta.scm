;; eta.scm
;;
;; A main program that imitates the behavior of Embelin Time
;; Assistant. No full feature parity.
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

(define-module (feta eta)
  :export (etaish-main
           etadb-load
           etadb-save
           etadb-line->session
           session->etaui-line
           session->etadb-line)
  :use-module (srfi srfi-19)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 rdelim)
  :use-module (feta localtime)
  :use-module (feta nih)
  :use-module (feta session)
  :use-module (feta time-range)
  :use-module (feta time-range-match))

(define (session->etaui-line session)
  (let* ((finished (session-finished? session))
         (startt (session-start session))
         (endt (if finished (session-end session) (now)))

         (start (time-utc->date startt))
         (end (time-utc->date endt))

         (desc (session-description session))
         (dur (time-second
               (time-range-duration
                (make-time-range startt endt))))

         (form "~Y-~m-~d ~H:~M:~S")
         (form2 " Until now (~H:~M) "))

    (string-append
     "event: "
     (date->string start form)
     " - "
     (date->string end (if finished form form2))
     (if finished
         " = "
         " =[")
     (duration->string dur)
     (if finished
         " '"
         "]'")
     desc
     "'")))

(define (etadb-line->session line)
  (catch #t
         (lambda ()
           (let* ((lp (open-input-string line))
                  (start (string->number (read-delimited ";" lp 'trim)))
                  (endstr (read-delimited ";" lp 'trim))
                  (end (if (equal? (string-length endstr) 0)
                           #f
                           (string->number endstr)))
                  (desc (string-trim-right (read-line lp 'trim) #\;)))
             (make-session desc (make-time-range start end))))
         (lambda _ (throw 'bad-db-line line))))

(define (session->etadb-line session)
  (string-append
   (number->string
    (time-second
     (session-start session)))
   ";"
   (if (session-finished? session)
       (number->string
        (time-second
         (session-end session)))
       "")
   ";"
   (session-description session)
   ";"))

(define (etadb-load in-port)
  (let ((line (read-line in-port 'trim)))
    (if
     ;; On EOF...
     (eof-object? line)
     ;; ... return empty list
     '()
     ;; Otherwise we try to parse the line
     (catch 'bad-db-line
            (lambda ()
              (cons
               (etadb-line->session line)
               (etadb-load in-port)))
            (lambda _
              (etadb-load in-port))))))

(define (etadb-save out-port sessions)
  (for-each
   (lambda (session)
     (display (session->etadb-line session) out-port)
     (newline out-port))
   sessions))

(define (session-starts-before? s0 s1)
  (time<? (session-start s0)
          (session-start s1)))


(define (sessions-closed-at ct sessions)
  (map (lambda (session)
         (if (time-range-complete?
              (session-time-range session))
             session
             (make-session-from session #:end ct)))
       sessions))

;; Option specification for getopt-long
;; The specification format seems to leave
;; something to be desired. See comments
;; interleaved in the spec.
(define option-spec
  '(;; First the trivial ones:
    ;; --version and --help
    (version
     (value #f))
    (help
     (value #f)
     (single-char #\h))

    ;; Now onto our actual functionality.

    ;; Start and end should be orthogonal, but
    ;; there seems to be no way to declare it here.
    (start
     (value #f)
     (single-char #\s))
    (end
     (value #f)
     (single-char #\e))

    ;; Description should only make sense when starting a
    ;; session, and possibly also when ending it. Though the
    ;; latter option seems potentially confusing for users.
    (description

     ;; Requires a free-form text description as argument
     (value #t)
     (single-char #\d))

    ;; Time defaults to current time, but this lets the
    ;; user give the program some other point in time.
    ;; Only makes sense when starting or ending.
    (time
     ;; Requires something that matches into a time
     ;; range. The start of range is used as time when
     ;; starting/ending.
     (value #t)
     (single-char #\t))

    ;; Sets the database location. Always applicable
    (file
     (value #t)
     (single-char #\f))

    ;; Only considers sessions fitting inside the specified
    ;; range. Requires a time range specification.
    (filter-time
     (value #t))))

(define (dump-sessions sessions)
  (let ((total 0))
    (for-each
     (lambda (session)
       (set! total (+ total
                      (time-second
                       (time-range-duration
                        (make-time-range
                         (session-start session)
                         (if (session-finished? session)
                             (session-end session) (now)))))))

       (display (session->etaui-line session))
       (newline))
     sessions)
    (display (string-append (duration->string total)
                            " in total.\n"))))


(define (countv k l . prevs)
  (let ((prev (if (null? prevs) 0 (car prevs))))
    (if (null? l) prev
        (countv k
                (cdr l)
                (+ (if (equal? k (car l)) 1 0) prev)))))

(define (string->session-filter str)
  (let ((tr (string->time-range str)))
    (if (not (time-range? tr))
        (throw 'invalid-time-range-spec str)
        ;; Check if session starts inside the time range
        (lambda (session)
          (time-range-contains?
           (session-start session) tr)))))

(define (etaish-main argv)
  (let* ((opts (getopt-long argv option-spec))
         (want-help  (option-ref opts 'help  #f))
         (want-start (option-ref opts 'start #f))
         (want-end   (option-ref opts 'end   #f))

         (requested-time
          (let ((timearg (option-ref opts 'time #f)))
            (if timearg
                (let ((tr (string->time-range timearg)))
                  (if tr
                      (time-range-start tr)

                      ;; Could not parse
                      (throw 'could-not-parse-time timearg)))
                ;; Time was not specified
                (now))))

         (db-location
          (option-ref opts 'file
                      ;; We default to ${HOME}/.ttdb
                      (string-append (getenv "HOME") "/.ttdb")))

         ;; Now this is a bit heavy. We always load the
         ;; whole database.
         (old-db (sort
                  (etadb-load (open-input-file db-location))
                  session-starts-before?))
         (descr-for-new
          (option-ref opts 'description
                      (if (null? old-db)
                          "PROJECT"
                          (session-description (car (last-pair old-db))))))
         (tf-spec (option-ref opts 'filter-time 'take-all)))

    (cond
     (want-help
      (display "Write the fine manual\n"))

     ((< 1
         (length (filter identity (list want-end
                                        want-start))))
      (display "Please only pick one operation of --end and --start\n"))

     (want-start
      (etadb-save
       (open-output-file db-location)
       (sort (cons (make-session descr-for-new
                                 (make-time-range requested-time))
                   (sessions-closed-at requested-time old-db))
             session-starts-before?)))

     (want-end
      (etadb-save
       (open-output-file db-location)
       (sort (sessions-closed-at requested-time old-db)
             session-starts-before?)))
     (#t
      ;; Display of sessions is our default action
      (dump-sessions (if (eq? tf-spec 'take-all) old-db
                         (filter
                          (string->session-filter tf-spec)
                          old-db)))))))
