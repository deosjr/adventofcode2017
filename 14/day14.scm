#! /usr/bin/scheme --script
;; If going the --program route, this is how to import
;;(import (lib lib) (rnrs))
(load "lib/lib.scm")
(load "10/knothash.scm")

(define input "oundnydw")

;; where n is an 8-bit unsigned integer
(define (ones n)
  (letrec ((count-ones 
    (lambda (x e)
      (let ((expo (expt 2 e)))
        (cond
          [(= e 0) (div x 1)]
          [(<= expo x) (+ 1 (count-ones (- x expo) (- e 1)))]
          [(> expo x) (count-ones x (- e 1))]
        )))))
    (count-ones n 7)))

(define (key x)
  (string-append (string-append input "-") (number->string x)))
(define keys
  (map key (iota 128)))
(define total-ones
  (map (lambda (x) (map ones x)) (map knothash (map knothashkey keys))))
(define p1
  (apply + (map (lambda (x) (apply + x)) total-ones)))
(write-part1 p1)

(define (make-coord x y)
  (list 'coord x y))

(define coords (make-hashtable equal-hash equal?))

(define coordlist '())
(define (store-coord x y)
  (let ((c (make-coord x y)))
  (set! coordlist (cons c coordlist))
  (hashtable-set! coords c #t)))

(define (store-ones num x y)
  (letrec ((count-ones 
    (lambda (n e)
      (let ((expo (expt 2 e)))
        (cond
          [(= e 0) (if (= n 1) (store-coord (+ x 7) y))]
          [(<= expo n) (store-coord (+ x (- 7 e)) y) (count-ones (- n expo) (- e 1))]
          [(> expo n) (count-ones n (- e 1))]
        )))))
    (count-ones num 7)))

(define (store-for-key key y)
  (do ((nums key (cdr nums)) (x 0 (+ x 8)))
    ((null? nums))
    (store-ones (car nums) x y)
  ))

(define (khash k) (knothash (knothashkey (key k))))
(map (lambda (i) (store-for-key (khash i) i)) (iota 128))

(define (coord? c)
  (hashtable-ref coords c #f))

(define (neighbours c)
  (let ((x (cadr c)) (y (caddr c)))
    (list
      (make-coord (- x 1) y)
      (make-coord (+ x 1) y)
      (make-coord x (- y 1))
      (make-coord x (+ y 1))
  )))

(define (remove-region c)
  (hashtable-delete! coords c)
  (map (lambda (x) (remove-region x)) (filter coord? (neighbours c))))

(define p2
  (do ((clist coordlist (filter coord? clist)) (regions 0 (+ 1 regions)))
    ((null? clist) regions)
    (remove-region (car clist))
  ))

(write-part2 p2)
