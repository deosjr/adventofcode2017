#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define input (string->list (read-entire-file 2)))

(define (take-until lst chlist)
  (if (member (car lst) chlist)
    (cons '() lst)
    (let ((next (take-until (cdr lst) chlist)))
      (cons (cons (car lst) (car next)) (cdr next))
      )
  ))

(define (list->number lst) (string->number (list->string lst)))

(define (p1 lst sum nmin nmax)
  (if (eq? lst '())
    sum
    (cond
        [(char=? (car lst) #\tab) (p1 (cdr lst) sum nmin nmax)]
        [(char=? (car lst) #\newline) (p1 (cdr lst) (+ sum (- nmax nmin)) 99999 0)]
        [else (let ((taken (take-until lst '(#\tab #\newline)) ))
            (let ((n (list->number (car taken))) (rem (cdr taken)))
              (p1 rem sum (min nmin n) (max nmax n))
            ))]
  )))

(write-part1 (p1 input 0 99999 0))

(define (evalrow row)
  (if (eq? row '())
    0
    (max (evalrow (cdr row)) (compare (car row) (cdr row)))
  ))

(define (compare c lst)
  (if (eq? lst '())
    0
    (let ((x (car lst)) (xs (cdr lst)))
      (if (> c x)
        (if (= (mod c x) 0) (div c x) (compare c xs))
        (if (= (mod x c) 0) (div x c) (compare c xs))
  ))))

(define (p2 lst row sum)
  (if (eq? lst '())
    sum
    (cond
        [(char=? (car lst) #\tab) (p2 (cdr lst) row sum)]
        [(char=? (car lst) #\newline) (p2 (cdr lst) '() (+ sum (evalrow row)))]
        [else (let ((taken (take-until lst '(#\tab #\newline)) ))
            (let ((n (list->number (car taken))) (rem (cdr taken)))
              (p2 rem (cons n row) sum)
            ))]
  )))

(write-part2 (p2 input '() 0))
