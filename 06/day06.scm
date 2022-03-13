#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define input (string->list (read-file-oneline 6)))
(define mem (make-eq-hashtable))

(define (take-until lst chlist)
  (if (eq? lst '())
    (cons '() '())
    (if (member (car lst) chlist)
      (cons '() lst)
      (let ((next (take-until (cdr lst) chlist)))
        (cons (cons (car lst) (car next)) (cdr next))
  ))))

(define (parse-mem lst i)
  (if (eq? lst '())
    #t
    (cond
        [(char=? (car lst) #\tab) (parse-mem (cdr lst) i)]
        [else (let ((taken (take-until lst '(#\tab)) ))
            (let ((n (string->number (list->string (car taken)))) (rem (cdr taken)))
              (hashtable-set! mem i n)
              (parse-mem rem (+ i 1)
            )))]
  )))
(parse-mem input 0)

(define (membanks) (map (lambda (i) (cons i (hashtable-ref mem i #f))) (iota 16)))
(define (config) (map cdr (membanks)))
(define (maxmem) (car (sort (lambda (x y) (> (cdr x) (cdr y))) (membanks))))

(define seen (make-hashtable equal-hash equal?))
(define (redistribute cycles)
  (let ((maxi (car (maxmem))) (maxblocks (cdr (maxmem))))
    (let ((n (div maxblocks 16)) (rem (mod maxblocks 16)))
      (hashtable-set! mem maxi 0)
      (map (lambda (x) (hashtable-set! mem x
        (+ (hashtable-ref mem x #f) (+ n (if (< (mod (+ x (- 15 maxi)) 16) rem) 1 0)))))
        (iota 16))
      (let ((v (hashtable-ref seen (config) #f)))
        (if (not (eq? #f v)) 
          (cons (+ cycles 1) (- cycles v))
          (begin
            (hashtable-set! seen (config) cycles)
            (redistribute (+ cycles 1))
  ))))))

(define ans (redistribute 0))
(write-part1 (car ans))
(write-part2 (cdr ans))
