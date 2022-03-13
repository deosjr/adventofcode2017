#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define (next-token line)
  (call-with-values
    (lambda () (read-token line))
    (lambda (type value start end) value)))

(define maxdist 0)

(define (distance q r s)
  (max (abs q) (abs r) (abs s)))

(define (p1 input q r s)
  (let ((ch (next-token input)) (dist (distance q r s)))
    (if (> dist maxdist) (set! maxdist dist))
    (cond
      [(eq? #!eof ch) dist]
      [(eq? 'unquote ch) (p1 input q r s)]
      [(eq? 'nw ch) (p1 input (- q 1) r (+ s 1))]
      [(eq? 'n ch) (p1 input q (- r 1) (+ s 1))]
      [(eq? 'ne ch) (p1 input (+ q 1) (- r 1) s)]
      [(eq? 'sw ch) (p1 input (- q 1) (+ r 1) s)]
      [(eq? 's ch) (p1 input q (+ r 1) (- s 1))]
      [(eq? 'se ch) (p1 input (+ q 1) r (- s 1))]
  )))

(write-part1 (p1 (open-input-string (read-file-oneline 11)) 0 0 0))
(write-part2 maxdist)
