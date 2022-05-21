#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define input 303)

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

; once more but without actually creating the list
(set! state 1) ; only length of the list this time
(set! position 0)
(define (step) (set! position (modulo (+ position (modulo input state)) state)))

(define ans 0)
(for-each (lambda (i) 
    (step)
    (if (= position 0) (set! ans state))
    (set! state (+ state 1))
    (set! position (+ position 1))
  ) (iota 50000000))

(write-part2 ans)
