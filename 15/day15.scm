#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define gena 699)
(define genb 124)

(define (next-until-mod g factor m)
  (let ((x (modulo (* g factor) 2147483647)))
    (if (= (modulo x m) 0) x (next-until-mod x factor m))))

(define (next generators)
  (let ((a (car generators)) (b (cdr generators)))
    (cons (next-until-mod a 16807 1) (next-until-mod b 48271 1))))

; 65536 is 2**16
(define (judge? generators)
  (let ((a (car generators)) (b (cdr generators)))
    (= (modulo a 65536) (modulo b 65536))
  ))

(define (p1 i gens f)
  (cond
    [(= i 0) 0]
    [else (let ((ngens (f gens)) (ni (- i 1)))
      (cond [(judge? ngens) (+ 1 (p1 ni ngens f))] [else (p1 ni ngens f)]))
    ]))

(write-part1 (p1 40000000 (cons gena genb) next))

(define (next2 generators)
  (let ((a (car generators)) (b (cdr generators)))
    (cons (next-until-mod a 16807 4) (next-until-mod b 48271 8))))

(write-part2 (p1 5000000 (cons gena genb) next2))
