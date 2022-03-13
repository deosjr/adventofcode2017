#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define (next-token line)
  (call-with-values
    (lambda () (read-token line))
    (lambda (type value start end) value)))

(define (tokens line)
  (let ((next (next-token line)))
    (if (eq? #!eof next)
      '()
      (cons next (tokens line)))))

(define pipes (make-eq-hashtable))

(define input (filehandle 12))
(do ((line (get-line input) (get-line input)))
  ((eq? #!eof line))
  (let ((parsed (tokens (open-input-string line))))
    (let ((program (car parsed)) (others (filter (lambda (x) (not (eq? 'unquote x))) (cddr parsed))))
      (hashtable-set! pipes program others))))

(define clique-set (make-eq-hashtable))
(define (update-clique n)
  (hashtable-set! clique-set n #t)
  (for-each
    (lambda (c)
      (if (not (hashtable-ref clique-set c #f))
        (update-clique c)))
    (hashtable-ref pipes n '())))

(update-clique 0)
(write-part1 (vector-length (hashtable-keys clique-set)))

(define cliques (make-hashtable equal-hash equal?))
(for-each
  (lambda (i)
    (begin
      (set! clique-set (make-eq-hashtable))
      (update-clique i)
      (hashtable-set! cliques (sort < (vector->list (hashtable-keys clique-set))) #t)
    ))
  (iota 2000))

(write-part2 (vector-length (hashtable-keys cliques)))
