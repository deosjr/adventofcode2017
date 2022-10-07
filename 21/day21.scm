#! /usr/bin/scheme --script
(load "lib/lib.scm")

; lets me write some prolog style code, not sure yet if thats a good thing
(define-syntax let-destr
  (syntax-rules ()
    ((_ (x y) e1 e2 ...) (let-values (( x (apply values y) )) e1 e2 ... ))))

(define (split-on-arrow s)
  (sscanf s "%s => %s"))

(define (parse2x2 s)
  (let-values (((p0 p1 p2 p3) (sscanf s "%c%c/%c%c")))
    (list (list p0 p1) (list p2 p3))))
(define (parse3x3 s)
  (let-values (((p0 p1 p2 p3 p4 p5 p6 p7 p8) (sscanf s "%c%c%c/%c%c%c/%c%c%c")))
    (list (list p0 p1 p2) (list p3 p4 p5) (list p6 p7 p8))))
(define (parse4x4 s)
  (let-values (((p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15) (sscanf s "%c%c%c%c/%c%c%c%c/%c%c%c%c/%c%c%c%c")))
    (list (list p0 p1 p2 p3) (list p4 p5 p6 p7) (list p8 p9 p10 p11) (list p12 p13 p14 p15))))

; the idea is to map each pattern to a canonical version under flip/rotation
; by generating all 8 possibilities, interpreting . as 0 and # as 1, then choose smallest

; rotate 90 degrees clockwise
(define (rotate pattern)
  (let ((size (length pattern)))
    (cond
      [(eq? size 2)
       (let-destr ((row1 row2) pattern)
         (let-destr ((p0 p1) row1)
         (let-destr ((p2 p3) row2)
           (list (list p2 p0) (list p3 p1))
       )))]
      [(eq? size 3)
       (let-destr ((row1 row2 row3) pattern)
         (let-destr ((p0 p1 p2) row1)
         (let-destr ((p3 p4 p5) row2)
         (let-destr ((p6 p7 p8) row3)
           (list (list p6 p3 p0) (list p7 p4 p1) (list p8 p5 p2))
      ))))])))

; flip or mirror the pattern
(define (flip pattern)
  (map reverse pattern))

(define (print-pattern pattern)
  (for-each (lambda (row) (printf "~w\n" (list->string row))) pattern))

(define (pattern->string pattern)
  (list->string (apply append pattern)))

(define (string->pattern s)
  (let* ((l (string->list s))
         (size (length l)))
    (cond
      [(eq? size 4)
        (let-destr ((p0 p1 p2 p3) l)
          (list (list p0 p1) (list p2 p3)))]
      [(eq? size 9)
        (let-destr ((p0 p1 p2 p3 p4 p5 p6 p7 p8) l) 
          (list (list p0 p1 p2) (list p3 p4 p5) (list p6 p7 p8)))])))

; returns the canonical string representation of the pattern
(define (canonical pattern)
  (let* ((rot90 (rotate pattern))
         (rot180 (rotate rot90))
         (rot270 (rotate rot180))
         (flipped (flip pattern))
         (flip90 (flip rot90))
         (flip180 (flip rot180))
         (flip270 (flip rot270))
         (patterns (list pattern rot90 rot180 rot270 flipped flip90 flip180 flip270)))
  (car (sort string>? (map pattern->string patterns)))))

(define input (filehandle 21))

(define rules (make-hashtable equal-hash equal?))
(do ((line (get-line input) (get-line input)))
  ((eq? #!eof line))
  (let-values (((from to) (split-on-arrow line)))
    (let ((parsedfrom (if (eq? 5 (string-length from)) (parse2x2 from) (parse3x3 from)))
          (parsedto  (if (eq? 11 (string-length to))   (parse3x3 to)   (parse4x4 to ))))
      (hashtable-set! rules (canonical parsedfrom) parsedto)
  )))

; store the pixels of a pattern in a (coord)->pixel table
; upper left hand corner coordinates are given as an offset
(define (store table pattern ulhcx ulhcy)
  (do ((y ulhcy (+ 1 y))
       (rows pattern (cdr rows)))
    ((null? rows))
    (do ((x ulhcx (+ 1 x))
         (row (car rows) (cdr row)))
      ((null? row))
      (hashtable-set! table (cons x y) (car row)))))

(define (retrieve size table ulhcx ulhcy)
  (map (lambda (y)
    (map (lambda (x)
      (hashtable-ref table (cons (+ ulhcx x) (+ ulhcy y)) #f))
    (iota size)))
  (iota size)))

(define start (parse3x3 ".#./..#/###"))

(define (iterate grid gridsize patternsize)
  (let ((range (iota (/ gridsize patternsize)))
        (nextsize (+ patternsize 1))
        (nextgrid (make-hashtable equal-hash equal?)))
    (map (lambda (y)
      (map (lambda (x)
        (let ((key (canonical (retrieve patternsize grid (* x patternsize) (* y patternsize)))))
        (store nextgrid (hashtable-ref rules key #f) (* x nextsize) (* y nextsize))))
      range))
    range)
    nextgrid))

(define (gridscore grid)
  (length (remq #\. (vector->list (hashtable-values grid)))))

(define (patternscore pattern)
  (apply + (map (lambda (l) (length (remq #\. l))) pattern)))

(define (iterate-rec grid size n)
  (if (eq? n 0)
    (gridscore grid)
    (let* ((even (eq? (modulo size 2) 0))
           (newgrid (iterate grid size (if even 2 3)))
           (newsize (if even (/ (* size 3) 2) (/ (* size 4) 3))))
      (iterate-rec newgrid newsize (- n 1)))))

(define (pattern->grid pattern)
  (let ((grid (make-hashtable equal-hash equal?)))
    (store grid pattern 0 0)
    grid))

(write-part1 (iterate-rec (pattern->grid start) 3 5))

; we go size 3->4->6 (also div 2!) ->9 and start again,
; so each 3x3 square becomes 9 3x3 squares after 3 iterations
; doing operation size 3 then 2x size 2
; after that, we have 9 3x3 grids that grow independently so we can iterate on that
; 18 iterations means 6x3 iterations like that, but we can now use caching!
(define (iterate-3 grid)
  (iterate (iterate (iterate grid 3 3) 4 2) 6 2))

(define (9x9->9x3x3 grid)
  (map (lambda (c) (retrieve 3 grid (car c) (cadr c)))
    '((0 0) (3 0) (6 0) (0 3) (3 3) (6 3) (0 6) (3 6) (6 6))))

(define (p2 pattern3x3 n)
  (if (eq? n 0)
    (patternscore pattern3x3)
    (apply + (map (lambda (p) (p2 p (- n 1))) (9x9->9x3x3 (iterate-3 (pattern->grid pattern3x3)))))))

; TODO this can be faster with memoisation
(write-part2 (p2 start 6))
