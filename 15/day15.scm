#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define gena 699)
(define genb 124)
;(define gena 65)
;(define genb 8921)

(define (next generators)
  (let ((a (car generators)) (b (cdr generators)))
    (cons (modulo (* a 16807) 2147483647)
        (modulo (* b 48271) 2147483647))))

(write-part1 (cons gena genb))
(write-part1 (next (cons gena genb)))

; 65536 is 2**16
(define (judge? generators)
  (let ((a (car generators)) (b (cdr generators)))
    (= (modulo a 65536) (modulo b 65536))
  ))

(define (p1 i gens)
  (cond
    [(= i 0) 0]
    [else (let ((ngens (next gens)) (ni (- i 1)))
      (cond [(judge? ngens) (+ 1 (p1 ni ngens))] [else (p1 ni ngens)]))
    ]))

(write-part1 (p1 40000000 (cons gena genb)))
