#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define input (filehandle 20))

; (sscanf "p=<1,2,3>" "p=<%d,%d,%d>") -> (1 2 3)
; silently fails and returns whatever succeeded so far in a list
(define (sscanf str fmt)
  (define strhandle (open-input-string str))
  (define fmthandle (open-input-string fmt))
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
  (sscanf-rec))

(define (parseline line)
  (let ((numbers (sscanf line "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>")))
    (apply (lambda (p0 p1 p2 v0 v1 v2 a0 a1 a2 . x)
    (list (list p0 p1 p2) (list v0 v1 v2) (list a0 a1 a2))) numbers)))

(define (vector-len v)
  (let ((v0 (car v)) (v1 (cadr v)) (v2 (caddr v)))
    (+ (abs v0) (abs v1) (abs v2))))

(define (vector-add p q)
  (list (+ (car p) (car q)) (+ (cadr p) (cadr q)) (+ (caddr p) (caddr q))))

(define (vector-mul s v)
  (let ((v0 (car v)) (v1 (cadr v)) (v2 (caddr v)))
    (list (* v0 s) (* v1 s) (* v2 s))))

; part 1 is finding the particle with smallest acceleration
; and accounting for starting positions some others might be closer at start
(define p1 (cons 0 9999999))

(do ((line (get-line input) (get-line input))
     (linenum 0 (+ 1 linenum)))
  ((eq? #!eof line))
  (let* ((parsed (parseline line))
         (p (car parsed))
         (v (cadr parsed))
         (a (caddr parsed))
         (sum (vector-add (vector-add p (vector-mul 1000 v)) (vector-mul (/ (* 1000 1001) 2) a)))
         (len (vector-len sum))
         )
    (if (< len (cdr p1))
      (set! p1 (cons linenum len)))))

(write-part1 (car p1))
