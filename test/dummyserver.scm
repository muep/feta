#!/bin/sh
# -*- scheme -*-
exec guile $GUILE_FLAGS -e main -s "$0" "$@"
!#
;; dummyserver.scm
;;
;; Builds an HTML report from data in standard input
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

(use-modules (sxml simple))

(use-modules (web request))
(use-modules (web response))
(use-modules (web server))
(use-modules (web uri))

(use-modules (feta eta))
(use-modules (feta xml))

(define (make-hdlr db)
  (lambda (req req-body)
    (let* ((outs (open-output-string)))
      (sxml->xml (sessions->xhtml db) outs)
      (values '((content-type . (text/html)))
              (get-output-string outs)))))

(define (path-components req)
  (split-and-decode-uri-path (uri-path (request-uri req))))

(define (get-dbname comps)
  (if (< 2 (length comps))
      #f
      (car comps)))

(define (get-dbtype comps . rest)
  (let ((allowed-types (if (null? rest) '(html) (car rest)))
        (requested-type (cond ((equal? 1 (length comps)) 'html)
                              ((equal? 2 (length comps)) (string->symbol (cadr comps)))
                              (#t #f))))
    (if (member requested-type allowed-types)
        requested-type
        (throw 'dbtype-not-available))))

(define (handle-unsupported req req-body)
  (let* ((comps (path-components req)))
    (display "Handling unsupported\n")
    (values '((content-type . (text/plain)))
            "I do not know what I am doing")))

(define (handle-get req req-body)
  (let* ((get-dbtypes '(html xml))
         (out-types '((html . (text/html))
                      (xml . (application/xml))))
         (comps (path-components req))
         (dbname (get-dbname comps))
         (dbtype (get-dbtype comps get-dbtypes))
         (in-port (open-input-file dbname))
         (db (etadb-load in-port))
         (out-port (open-output-string))
         (out-data #f))

    ;; This was already read into db
    (close-port in-port)

    (case dbtype
      ('html (sxml->xml (sessions->xhtml db) out-port))
      ('xml (sxml->xml (sessions->sxml db) out-port)))

    (set! out-data (get-output-string out-port))
    (close-port out-port)

    (values `((content-type . (,(cadr (assoc dbtype out-types)))))
            out-data)))
(define handle-post handle-unsupported)

(define (hdlr2 req req-body)
  (case (request-method req)
    ('GET (handle-get req req-body))
    ('POST (handle-post req req-body))
    (else (handle-unsupported))))

(define (main args)
  (display "Start serving.\n")
  (run-server hdlr2))
