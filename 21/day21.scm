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

; practising macros a bit more, here to shorthand table lookup by xy-coordinates
(define-syntax t
  (syntax-rules ()
    ((_ table x y) (hashtable-ref table (cons x y) #f) )))

(define (retrieve2x2 table x y)
  (list
    (list (t table x y) (t table (+ x 1) y))
    (list (t table x (+ y 1)) (t table (+ x 1) (+ y 1)))))

(define (retrieve3x3 table x y)
  (list
    (list (t table x y) (t table (+ x 1) y) (t table (+ x 2) y))
    (list (t table x (+ y 1)) (t table (+ x 1) (+ y 1)) (t table (+ x 2) (+ y 1)))
    (list (t table x (+ y 2)) (t table (+ x 1) (+ y 2)) (t table (+ x 2) (+ y 2)))))

; we go size 3->4->6 (also div 2!) ->9 and start again,
; so each 3x3 square becomes 9 3x3 squares after 3 iterations
; doing operation size 3 then 2x size 2

(define grid (make-hashtable equal-hash equal?))

; ITERATION 0
(define start (parse3x3 ".#./..#/###"))
(store grid start 0 0)

; ITERATION 1
(define grid1 (make-hashtable equal-hash equal?))
(store grid1 (hashtable-ref rules (canonical (retrieve3x3 grid 0 0)) #f) 0 0)

; ITERATION 2
(define grid2 (make-hashtable equal-hash equal?))
(store grid2 (hashtable-ref rules (canonical (retrieve2x2 grid1 0 0)) #f) 0 0)
(store grid2 (hashtable-ref rules (canonical (retrieve2x2 grid1 2 0)) #f) 3 0)
(store grid2 (hashtable-ref rules (canonical (retrieve2x2 grid1 0 2)) #f) 0 3)
(store grid2 (hashtable-ref rules (canonical (retrieve2x2 grid1 2 2)) #f) 3 3)

; ITERATION 3
(define grid3 (make-hashtable equal-hash equal?))
(store grid3 (hashtable-ref rules (canonical (retrieve2x2 grid2 0 0)) #f) 0 0)
(store grid3 (hashtable-ref rules (canonical (retrieve2x2 grid2 2 0)) #f) 3 0)
(store grid3 (hashtable-ref rules (canonical (retrieve2x2 grid2 4 0)) #f) 6 0)
(store grid3 (hashtable-ref rules (canonical (retrieve2x2 grid2 0 2)) #f) 0 3)
(store grid3 (hashtable-ref rules (canonical (retrieve2x2 grid2 2 2)) #f) 3 3)
(store grid3 (hashtable-ref rules (canonical (retrieve2x2 grid2 4 2)) #f) 6 3)
(store grid3 (hashtable-ref rules (canonical (retrieve2x2 grid2 0 4)) #f) 0 6)
(store grid3 (hashtable-ref rules (canonical (retrieve2x2 grid2 2 4)) #f) 3 6)
(store grid3 (hashtable-ref rules (canonical (retrieve2x2 grid2 4 4)) #f) 6 6)

; ITERATION 4
(define grid4 (make-hashtable equal-hash equal?))
(store grid4 (hashtable-ref rules (canonical (retrieve3x3 grid3 0 0)) #f) 0 0)
(store grid4 (hashtable-ref rules (canonical (retrieve3x3 grid3 3 0)) #f) 4 0)
(store grid4 (hashtable-ref rules (canonical (retrieve3x3 grid3 6 0)) #f) 8 0)
(store grid4 (hashtable-ref rules (canonical (retrieve3x3 grid3 0 3)) #f) 0 4)
(store grid4 (hashtable-ref rules (canonical (retrieve3x3 grid3 3 3)) #f) 4 4)
(store grid4 (hashtable-ref rules (canonical (retrieve3x3 grid3 6 3)) #f) 8 4)
(store grid4 (hashtable-ref rules (canonical (retrieve3x3 grid3 0 6)) #f) 0 8)
(store grid4 (hashtable-ref rules (canonical (retrieve3x3 grid3 3 6)) #f) 4 8)
(store grid4 (hashtable-ref rules (canonical (retrieve3x3 grid3 6 6)) #f) 8 8)

; ITERATION 5
(define grid5 (make-hashtable equal-hash equal?))
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 0 0)) #f) 0 0)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 2 0)) #f) 3 0)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 4 0)) #f) 6 0)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 6 0)) #f) 9 0)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 8 0)) #f) 12 0)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 10 0)) #f) 15 0)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 0 2)) #f) 0 3)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 2 2)) #f) 3 3)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 4 2)) #f) 6 3)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 6 2)) #f) 9 3)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 8 2)) #f) 12 3)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 10 2)) #f) 15 3)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 0 4)) #f) 0 6)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 2 4)) #f) 3 6)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 4 4)) #f) 6 6)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 6 4)) #f) 9 6)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 8 4)) #f) 12 6)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 10 4)) #f) 15 6)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 0 6)) #f) 0 9)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 2 6)) #f) 3 9)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 4 6)) #f) 6 9)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 6 6)) #f) 9 9)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 8 6)) #f) 12 9)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 10 6)) #f) 15 9)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 0 8)) #f) 0 12)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 2 8)) #f) 3 12)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 4 8)) #f) 6 12)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 6 8)) #f) 9 12)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 8 8)) #f) 12 12)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 10 8)) #f) 15 12)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 0 10)) #f) 0 15)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 2 10)) #f) 3 15)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 4 10)) #f) 6 15)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 6 10)) #f) 9 15)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 8 10)) #f) 12 15)
(store grid5 (hashtable-ref rules (canonical (retrieve2x2 grid4 10 10)) #f) 15 15)

(write-part1 (length (remq #\. (vector->list (hashtable-values grid5)))))
