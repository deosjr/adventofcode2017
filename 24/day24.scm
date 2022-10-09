#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define (read-components)
  (define (readline input)
    (let ((line (get-line input)))
      (if (eq? line #!eof) '()
        (let-values (((x y) (sscanf line "%d/%d")))
          (cons (cons x y) (readline input))))))
  (readline (filehandle 24)))

(define (match-port? pins)
  (lambda (component)
    (let ((a (car component)) (b (cdr component)))
      (or (eq? pins a) (eq? pins b)))))

(define (other-port pins c)
  (let ((a (car c)) (b (cdr c)))
    (if (eq? pins a) b a)))

(define (score c)
  (let ((a (car c)) (b (cdr c)))
    (+ a b)))

; returns a list of lists, one for each component matching port n
; each list has the matching port as head and the others as tail
(define (test n components)
  (let-values (((match nonmatch) (partition (match-port? n) components)))
    (map (lambda (c)
      (cons c (append (remove c match) nonmatch))
      ) match)))

(define (p1 n components)
  (if (null? components) 0
    (let ((rec 
      (map (lambda (lst)
        (+ (score (car lst)) (p1 (other-port n (car lst)) (cdr lst)))
      ) (test n components))))
      (if (null? rec) 0 (apply max rec)))))

(write-part1 (p1 0 (read-components)))

; for part 2, just add a million for each added component to give
; higher score to longer bridges. print modulo 1 million for result
(define (p2 n components)
  (if (null? components) 0
    (let ((rec 
      (map (lambda (lst)
        (+ (score (car lst)) 1000000 (p2 (other-port n (car lst)) (cdr lst)))
      ) (test n components))))
      (if (null? rec) 0 (apply max rec)))))

(write-part2 (modulo (p2 0 (read-components)) 1000000))
