#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define input (string->list (read-entire-file 4)))

(define (take-until lst chlist)
  (if (member (car lst) chlist)
    (cons '() lst)
    (let ((next (take-until (cdr lst) chlist)))
      (cons (cons (car lst) (car next)) (cdr next))
      )
  ))

;; NOTE: eval new-dict each use!
(define (new-dict) (make-hashtable string-hash string=?))
(define (evalrow row dict)
  (if (eq? row '())
    1
    (cond 
      [(hashtable-ref dict (car row) #f) 0]
      [else (hashtable-set! dict (car row) #t) (evalrow (cdr row) dict)]
  )))

(define (p1 lst row sum)
  (if (eq? lst '())
    sum
    (cond
        [(char=? (car lst) #\space) (p1 (cdr lst) row sum)]
        [(char=? (car lst) #\newline) (p1 (cdr lst) '() (+ sum (evalrow row (new-dict)))) ]
        [else (let ((taken (take-until lst '(#\space #\newline)) ))
            (let ((s (list->string (car taken))) (rem (cdr taken)))
              (p1 rem (cons s row) sum)
            ))]
  )))

(write-part1 (p1 input '() 0))

(define (p2 lst row sum)
  (if (eq? lst '())
    sum
    (cond
        [(char=? (car lst) #\space) (p2 (cdr lst) row sum)]
        [(char=? (car lst) #\newline) (p2 (cdr lst) '() (+ sum (evalrow row (new-dict)))) ]
        [else (let ((taken (take-until lst '(#\space #\newline)) ))
            (let ((s (list->string (sort char<? (car taken)))) (rem (cdr taken)))
              (p2 rem (cons s row) sum)
            ))]
  )))

(write-part2 (p2 input '() 0))
