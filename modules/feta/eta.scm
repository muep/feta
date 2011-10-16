;; eta.scm
;;
;; A main program that imitates the behavior of
;; Embelin Time Assistant

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
  :use-module (feta session)
  :use-module (feta time-range)
  :use-module (feta time-range-match))

(define (zpad str)
  (if (>= (string-length str) 2)
      str
      (zpad (string-append "0" str))))

;; This might live a happy life elsewhere...
(define duration->string
  (lambda (seconds)
    (let* ((hours (floor (/ seconds 3600)))
           (remaining (modulo seconds 3600))
           (mins (floor (/ remaining 60))))
      (string-append (zpad (number->string hours)) ":"
                     (zpad (number->string mins))))))

(define (session->etaui-line session)
  (let ((start (time-utc->date
                (time-range-start
                 (session-time-range session))))
        (end (time-utc->date
                (time-range-start
                 (session-time-range session))))
        (desc (session-description session))
        (dur (time-second
              (time-range-duration
               (session-time-range session))))
        (form "~Y-~m-~d ~H:~M:~S"))

    (string-append
     "event: "
     (date->string start form)
     " - "
     (date->string start form)
     " = "
     (duration->string dur)
     " '"
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
     (time-range-start
      (session-time-range session))))
   ";"
   (number->string
    (time-second
     (time-range-end
      (session-time-range session))))
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

(define etaish-main
  (lambda (argv)
    (getopt-long argv option-spec)))
