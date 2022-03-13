#! /usr/bin/scheme --script
;; If going the --program route, this is how to import
;;(import (lib lib) (rnrs))
(load "lib/lib.scm")
(load "10/knothash.scm")

(define input "oundnydw")
;;(define input "flqrgnkx")

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

(define (store-coord x y)
  (hashtable-set! coords (make-coord x y) #t))

(define (store-ones num x y)
  (letrec ((count-ones 
    (lambda (n e)
      (let ((expo (expt 2 e)))
        (cond
          [(= e 0) (if (div n 1) (store-coord (+ x 7) y))]
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

(for-each (lambda (x) (write-part1 (hashtable-ref coords (make-coord x 8) #f))) (iota 16))
