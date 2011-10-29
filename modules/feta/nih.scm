;; nih.scm
;;
;; Random stuff for which it was not easy to find any other
;; sensible place.
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

(define-module (feta nih)
  :export (aget
           all
           duration->string
           get-kval
           uniquify))

;; Get from association list, or #f
(define (aget key l)
  (let ((c (assoc key l)))
    (if c (cdr c) #f)))

;; Returns #t if all items in l satisfy ok?,
;; Otherwise returns #f.
(define all
  (lambda (ok? l)
    (cond ((null? l) #t)
          ((not (ok? (car l))) #f)
          (#t (all ok? (cdr l))))))

;; Pads string with zeroes from the left until its length is
;; at least 2.
(define (zpad str)
  (if (>= (string-length str) 2)
      str
      (zpad (string-append "0" str))))

;; Makes a nice hh:mm representation for a duration given in
;; seconds.
(define (duration->string seconds)
  (let* ((hours (floor (/ seconds 3600)))
         (remaining (modulo seconds 3600))
         (mins (floor (/ remaining 60))))
    (string-append (zpad (number->string hours)) ":"
                   (zpad (number->string mins)))))

;; Fetches the first item from l whose previous item is eq
;; with k. If no item is found, returns #f.
(define (get-kval k l)
  (let ((kl (member k l)))
    (if (or (not (list? kl))
            (null? kl)
            (not (list? (cdr kl)))
            (null? (cdr kl)))
        #f (cadr kl))))

(define remove-adjacents
  (lambda (list match? prev)
    (if (null? list)
        '()
        (let ((head (car list))
              (tail (cdr list)))
          (if (match? head prev)
              (remove-adjacents tail match? head)
              (cons head (remove-adjacents tail match? head)))))))

;; Removes returns a list with duplicate _strings_
;; removed.
(define uniquify
  (lambda (list)
    (remove-adjacents (sort list string<?) equal? "")))
