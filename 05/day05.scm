#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define (init-dict file dict i)
  (let ((line (get-line file)))
    (if (not (eq? line #!eof))
      (begin
        (hashtable-set! dict i (string->number line))
        (init-dict file dict (+ i 1))
  ))))

(define jumps (make-eq-hashtable))
(init-dict (filehandle 5) jumps 0)

(define (p1 dict index steps)
  (let ((v (hashtable-ref dict index #f)))
    (if (eq? v #f)
      steps
      (begin
        (hashtable-set! dict index (+ v 1))
        (p1 dict (+ index v) (+ steps 1))
  ))))
(write-part1 (p1 jumps 0 0))

(define jumps (make-eq-hashtable))
(init-dict (filehandle 5) jumps 0)

(define (p2 dict index steps)
  (let ((v (hashtable-ref dict index #f)))
    (if (eq? v #f)
      steps
      (begin
        (if (> v 2)
          (hashtable-set! dict index (- v 1))
          (hashtable-set! dict index (+ v 1)))
        (p2 dict (+ index v) (+ steps 1))
  ))))
(write-part2 (p2 jumps 0 0))
