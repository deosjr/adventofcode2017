#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define input (filehandle 8))
(define registers (make-eq-hashtable))

(define (next-token line)
  (call-with-values
    (lambda () (read-token line))
    (lambda (type value start end) value)))

(define (tokens line)
  (let ((next (next-token line)))
    (if (eq? #!eof next)
      '()
      (cons next (tokens line)))))

(define maxvalue 0)

(do ((line (get-line input) (get-line input)))
  ((eq? #!eof line))
  (let-values (((x op y iff z comp w) (apply values (tokens (open-input-string line)))))
    (if (eq? op 'dec) (set! y (- y)))
    (let ((xvalue (hashtable-ref registers x 0)) (zvalue (hashtable-ref registers z 0)))
      (cond
        [(eq? comp '==) (if (= zvalue w) (hashtable-set! registers x (+ xvalue y)))]
        [(eq? comp '!=) (if (not (= zvalue w)) (hashtable-set! registers x (+ xvalue y)))]
        [(eq? comp '<=) (if (not (> zvalue w)) (hashtable-set! registers x (+ xvalue y)))]
        [(eq? comp '>=) (if (not (< zvalue w)) (hashtable-set! registers x (+ xvalue y)))]
        [(eq? comp '<) (if (< zvalue w) (hashtable-set! registers x (+ xvalue y)))]
        [(eq? comp '>) (if (> zvalue w) (hashtable-set! registers x (+ xvalue y)))])
      (let ((newvalue (hashtable-ref registers x 0)))
        (if (> newvalue maxvalue) (set! maxvalue newvalue))
      ))))

(write-part1 (vector-ref (vector-sort > (hashtable-values registers)) 0))
(write-part2 maxvalue)
