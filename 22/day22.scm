#! /usr/bin/scheme --script
(load "lib/lib.scm")

; 2d grid parsing / reading functions
(define (read-with-coordinates file func)
  (define (row x y lst)
    (if (not (null? lst)) (begin
      (func x y (car lst))
      (row (+ x 1) y (cdr lst)))))
  (define (columns y)
    (let ((line (get-line file)))
      (if (not (eq? line #!eof)) (begin
        (row 0 y (string->list line))
        (columns (+ y 1))))))
  (columns 0))

(define (do-for-coordinates xmin xmax ymin ymax func)
  (for-each (lambda (y) (for-each (lambda (x)
    (func (+ x xmin) (+ y ymin)))
  (iota (- xmax xmin))))
  (iota (- ymax ymin))))

(define (print-grid grid xmax ymax)
  (do ((y 0 (+ y 1)))
    ((eq? y ymax))
    (do-for-coordinates 0 xmax y (+ y 1) (lambda (x y)
      (printf "~c" (hashtable-ref grid (cons x y) #f))))
      (printf "\n")))

; reused from day 19
(define (add-coord p q)
  (cons (+ (car p) (car q)) (+ (cdr p) (cdr q))))

(define (turn-left d)
  (let ((dx (car d)) (dy (cdr d)))
    (cons dy (- dx))))

(define (turn-right d)
  (let ((dx (car d)) (dy (cdr d)))
    (cons (- dy) dx)))

(define (input)
  (define grid (make-hashtable equal-hash equal?))
  (read-with-coordinates (filehandle 22)
    (lambda (x y c) (hashtable-set! grid (cons x y) c)))
  grid)

(define (burst grid pos dir)
  (let* ((current (hashtable-ref grid pos #f))
         (infected (eq? current #\#))
         (newdir (if infected (turn-right dir) (turn-left dir)))
         (newpos (add-coord pos newdir)))
    (hashtable-set! grid pos (if infected #\. #\#))
    (values newpos newdir infected)))

(define (p1 grid pos dir n)
  (if (eq? n 0) 0
    (let-values (((newpos newdir infected) (burst grid pos dir)))
      (+ (if infected 0 1) (p1 grid newpos newdir (- n 1)))
    )))

(write-part1 (p1 (input) (cons 12 12) (cons 0 -1) 10000))
