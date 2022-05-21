#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define input 303)
;(define input 3)

; todo cons
(define state '(0))
(define position 0)

(define (steps) (modulo input (length state)))
(define (step) (set! position (modulo (+ position (steps)) (length state))))

(for-each (lambda (i) 
    (step)
    (let ((tail (list-tail state position))) (set-cdr! tail (cons (+ i 1) (cdr tail))))
    (set! position (+ position 1))
  ) (iota 2017))

(write-part1 (list-ref state (+ 1 position)))
