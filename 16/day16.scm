#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define (make-map) 
  (let ((m (make-hashtable equal-hash equal?)))
    (map (lambda (i) (hashtable-set! m i i)) (iota 16)) m))

; a ring is a triple of startposition, lookup-by-index, lookup-by-name
(define ring (list 0 (make-map) (make-map)))

(define (min-mod16 x y) (modulo (- x y) 16))
(define (plus-mod16 x y) (modulo (+ x y) 16))
(define (print-char c) (integer->char (+ c 97)))
(define (print-ring ring)
  (let ((startpos (car ring)) (m (cadr ring)))
    (list->string (map (lambda (i) (print-char (hashtable-ref m (plus-mod16 i startpos) #f))) (iota 16)))))

(define (spin x)
  (lambda (ring)
    (let ((startpos (car ring)))
      (set-car! ring (min-mod16 startpos x)))))

(define (exchange a b)
  (lambda (ring)
    (let ((startpos (car ring)) (mindex (cadr ring)) (mname (caddr ring)))
      (let ((ak (plus-mod16 a startpos)) (bk (plus-mod16 b startpos)))
        (let ((av (hashtable-ref mindex ak #f)) (bv (hashtable-ref mindex bk #f)))
          (hashtable-set! mindex ak bv)
          (hashtable-set! mindex bk av)
          (hashtable-set! mname bv ak)
          (hashtable-set! mname av bk))))))

(define (partner a b)
  (lambda (ring)
    (let ((startpos (car ring)) (mindex (cadr ring)) (mname (caddr ring)) (a (- (char->integer a) 97)) (b (- (char->integer b) 97)))
      (let ((av (hashtable-ref mname a #f)) (bv (hashtable-ref mname b #f)))
        (hashtable-set! mname a bv)
        (hashtable-set! mname b av)
        (hashtable-set! mindex av b)
        (hashtable-set! mindex bv a)))))

(define (dance ring funcs)
  (for-each (lambda (f) (f ring)) funcs))

;; im lazy so lets do string replacement in sed
(define input 
  (let ((raw
    (get-string-all (car (process 
      "cat 16/day16.input | sed -e 's/,/) /g' -e 's/x\\([0-9]\\)/,(exchange \\1/g' -e 's/p\\([a-z]\\)/,(partner \\1/g' -e 's/s\\([0-9]\\)/,(spin \\1/g' -e 's/\\// /g' -e 's/ \\([a-z]\\) / #\\\\\\1 /g' -e 's/\\([a-z]\\))/#\\\\\\1)/g'")))))
    (string-append "`(" (string-truncate! raw (- (string-length raw) 1)) "))")))

(define moves (eval (read (open-input-string input))))
(dance ring moves)
(write-part1 (print-ring ring))

(define dances (make-hashtable equal-hash equal?))
(hashtable-set! dances "abcdefghijklmnop" 0)
(hashtable-set! dances (print-ring ring) 1)

; finding out that the dance loops in 36 iterations: 
;(for-each (lambda (i) (begin (dance ring moves) 
  ;(let* ((str (print-ring ring)) (v (hashtable-ref dances str #f)))
    ;(if v
      ;(write-part2 (cons (+ i 2) v))
      ;(hashtable-set! dances (print-ring ring) (+ i 2))))))
  ;(iota 120))

(set! ring (list 0 (make-map) (make-map)))
(for-each (lambda (i) (dance ring moves)) (iota (modulo 1000000000 36)))
(write-part2 (print-ring ring))
