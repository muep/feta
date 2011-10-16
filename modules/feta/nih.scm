;; nih.scm
;;
;; Functions implemented for feta which probably
;; were Not Invented Here.

(define-module (feta nih)
  :export (aget
           all
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
