#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define input (read-entire-file 1))

;; last char of input is a newline; replace it to make the list 'circular'
(string-set! input (- (string-length input) 1) (string-ref input 0))
(define numbers (map (lambda (x) (- (char->integer x) 48)) (string->list input)))

(define (p1 prev rest sum)
  (if (eq? rest '())
    sum 
    (if (= prev (car rest))
      (p1 (car rest) (cdr rest) (+ sum prev))
      (p1 (car rest) (cdr rest) sum)
  )))

(write-part1 (p1 (car numbers) (cdr numbers) 0))

(define (split-at n lst)
  (if (= n 0)
    (cons '() lst)
    (let ((prev (split-at (- n 1) (cdr lst))))
      (cons (cons (car lst) (car prev)) (cdr prev))
      )
  ))

(define split (split-at (div (length numbers) 2) numbers))
(define first-half (car split))
(define second-half (car (split-at (- (length (cdr split)) 1) (cdr split))))

(define (p2 lst rotated sum)
  (if (eq? lst '())
    sum 
    (if (= (car lst) (car rotated))
      (p2 (cdr lst) (cdr rotated) (+ sum (car lst)))
      (p2 (cdr lst) (cdr rotated) sum)
  )))

(write-part2 (p2 (append first-half second-half) (append second-half first-half) 0))
