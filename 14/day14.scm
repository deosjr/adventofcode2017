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
          [(<= expo x) (+ (div x expo) (count-ones (- x expo) (- e 1)))]
          [(> expo x) (count-ones x (- e 1))]
        )))))
    (count-ones n 7)))

(define keys
  (map (lambda (x) (string-append (string-append input "-") (number->string x))) (iota 128)))
(define total-ones
  (map (lambda (x) (map ones x)) (map knothash (map knothashkey keys))))
(define p1
  (apply + (map (lambda (x) (apply + x)) total-ones)))
(write-part1 p1)

(define (make-coord x y)
  (list 'coord x y))

(define coords (make-hashtable equal-hash equal?))
(hashtable-set! coords (make-coord 1 1) #t)
(write-part1 (hashtable-ref coords '(coord 1 1) #f))
