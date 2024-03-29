#! /usr/bin/scheme --script
(load "lib/lib.scm")

;destructuring let macro (my first macro!)
; TODO multiple destructuring statements in one let
(define-syntax let-destr
  (syntax-rules ()
    ((_ (x y) e1 e2 ...) (let-values (( x (apply values y) )) e1 e2 ... ))))

; TODO move to lib
; (sscanf "p=<1,2,3>" "p=<%d,%d,%d>") -> (1 2 3)
; silently fails and returns whatever succeeded so far in a list
(define (sscanf str fmt)
  (define strhandle (open-input-string str)) (define fmthandle (open-input-string fmt))
  ; parse a string until space
  (define (scanstr)
    (define (scanstr-rec)
      (if (eq? #\space (peek-char strhandle)) '() (cons (read-char strhandle) (scanstr-rec))))
    (list->string (scanstr-rec)))
  ; parse an integer
  (define (scanint)
    (define minus
      (if (eq? #\- (peek-char strhandle))
        (read-char strhandle) #f))
    (define (scanint-rec)
      (if (char-numeric? (peek-char strhandle)) (cons (read-char strhandle) (scanint-rec)) '()))
    (let ((digits (scanint-rec)))
      (if (not minus)
        (string->number (list->string digits))
        (string->number (list->string (cons minus digits))))))
  ; parse a single character
  (define (scanchar) (read-char strhandle))
  (define (sscanf-rec)
    (let ((c (read-char fmthandle)))
      (cond
        [(eq? c #!eof) '()]
        [(eq? c #\%)
         (let ((verb (read-char fmthandle)))
           (cond
             [(eq? verb #\d) (cons (scanint) (sscanf-rec))]
             [(eq? verb #\s) (cons (scanstr) (sscanf-rec))]
             [(eq? verb #\c) (cons (scanchar) (sscanf-rec))]
             [else '()]
         ))]
        [else (if (eq? c (read-char strhandle)) (sscanf-rec) '())])))
  (apply values (sscanf-rec)))

(define (parseline line)
  (let-values (( (p0 p1 p2 v0 v1 v2 a0 a1 a2)  (sscanf line "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>")))
      (list (list p0 p1 p2) (list v0 v1 v2) (list a0 a1 a2))))

(define (vector-len v)
  (let-values (( (v0 v1 v2) (apply values v)))
    (+ (abs v0) (abs v1) (abs v2))))

(define (vector-add p q)
  (let-values (( (p0 p1 p2) (apply values p))
               ( (q0 q1 q2) (apply values q)))
    (list (+ p0 q0) (+ p1 q1) (+ p2 q2))))

(define (vector-mul s v)
  (let-values (( (v0 v1 v2) (apply values v)))
    (list (* v0 s) (* v1 s) (* v2 s))))

(define (parsed-input)
  (define (parse-input input)
    (let ((line (get-line input)))
      (if (eq? #!eof line) '()
        (cons (parseline line) (parse-input input)))))
  (parse-input (filehandle 20)))

; part 1 is finding the particle with smallest acceleration
; and accounting for starting positions some others might be closer at start
(define (p1)
  (define (p1-rec particles idx ans)
    (if (null? particles) (car ans)
      (let ((particle (car particles)))
        (let-values (( (p v a) (apply values particle)))
          (let* ((sum (vector-add (vector-add p (vector-mul 1000 v)) (vector-mul (/ (* 1000 1001) 2) a)))
                 (len (vector-len sum)))
            (if (< len (cdr ans))
              (p1-rec (cdr particles) (+ 1 idx) (cons idx len))
              (p1-rec (cdr particles) (+ 1 idx) ans)))))))
  (p1-rec (parsed-input) 0 (cons 0 99999999)))

(write-part1 (p1))

; part 2 is actually simulating everything for a bunch of steps (?)
(define (p2)
  (define (simulate particles n)
    (let ((m (make-hashtable equal-hash equal?))
          (filtered (remq 'collision particles)))
      (if (eq? n 0) filtered
        (begin 
          (for-each (lambda (par)
            (let-destr ((p v a) par)
              (let* ((v2 (vector-add v a))
                     (p2 (vector-add p v2)))
                (if (hashtable-ref m p2 #f)
                  (hashtable-set! m p2 'collision)
                  (hashtable-set! m p2 (list p2 v2 a)))))) filtered)
          (simulate
            (vector->list (hashtable-values m))
            (- n 1))))))
  (simulate (parsed-input) 1000))

(write-part2 (length (p2)))
