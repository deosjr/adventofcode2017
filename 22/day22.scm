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
  (let* ((current (hashtable-ref grid pos #\.))
         (infected (eq? current #\#))
         (newdir (if infected (turn-right dir) (turn-left dir)))
         (newpos (add-coord pos newdir)))
    (hashtable-set! grid pos (if infected #\. #\#))
    (values newpos newdir (not infected))))

(define (p1 grid pos dir n acc)
  (if (eq? n 0) acc
    (let-values (((newpos newdir infected) (burst grid pos dir)))
      (p1 grid newpos newdir (- n 1) (+ acc (if infected 1 0))))))

(write-part1 (p1 (input) (cons 12 12) (cons 0 -1) 10000 0))

; bit hacky to set state as a side-effect but otherwise would duplicate cond
(define (burst2 grid pos dir)
  (define (set-state s) (hashtable-set! grid pos s))
  (define (rev d) (turn-right (turn-right d)))
  (let* ((current (hashtable-ref grid pos #\.))
         (newdir (cond 
           [(eq? current #\.) (set-state #\W) (turn-left dir)]  ; clean
           [(eq? current #\W) (set-state #\#) dir]              ; weakened
           [(eq? current #\#) (set-state #\F) (turn-right dir)] ; infected
           [(eq? current #\F) (set-state #\.) (rev dir)]))      ; flagged
         (newpos (add-coord pos newdir)))
    (values newpos newdir (eq? current #\W))))

(define (p2 grid pos dir n acc)
  (if (eq? n 0) acc
    (let-values (((newpos newdir infected) (burst2 grid pos dir)))
      (p2 grid newpos newdir (- n 1) (+ acc (if infected 1 0))))))

(write-part2 (p2 (input) (cons 12 12) (cons 0 -1) 10000000 0))
