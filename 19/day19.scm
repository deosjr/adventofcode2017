#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define diagram (make-hashtable equal-hash equal?))
(define input (filehandle 19))

(define pos (cons 0 0))  ;set when parsing input
(define dir (cons 0 1))  ;down

(define (add-coord p q)
  (cons (+ (car p) (car q)) (+ (cdr p) (cdr q))))

(define (turn-left d)
  (let ((dx (car d)) (dy (cdr d)))
    (cons dy (- dx))))

(define (turn-right d)
  (let ((dx (car d)) (dy (cdr d)))
    (cons (- dy) dx)))

(define (lookup p)
  (hashtable-ref diagram p #f))

(define (parse-line line y)
  (do ((lst (string->list line) (cdr lst))
       (x 0 (+ x 1)))
    ((null? lst)) 
    (let ((char (car lst)))
      (if (not (eq? char #\space))
        (begin
          (if (and (eq? y 0) (eq? char #\|)) (set! pos (cons x y)))
          (hashtable-set! diagram (cons x y) char))))))

(do ((line (get-line input) (get-line input))
     (y 0 (+ y 1)))
    ((eq? #!eof line)) (parse-line line y))

(define p1 '())
(define p2 0)

(do ((char (lookup pos) (lookup pos))
     (steps 0 (+ 1 steps)))
     ((not char) (set! p1 (list->string (reverse p1))) (set! p2 steps))
     (if (eq? char #\+)
       (let ((left (add-coord pos (turn-left dir)))
             (right (add-coord pos (turn-right dir))))
         (if (lookup left) (begin (set! pos left) (set! dir (turn-left dir)))
           (begin (set! pos right) (set! dir (turn-right dir)))))
       (begin
         (if (not (or (eq? char #\|) (eq? char #\-))) (set! p1 (cons char p1)))
         (set! pos (add-coord pos dir)))))

(write-part1 p1)
(write-part2 p2)
