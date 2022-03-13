#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define input (filehandle 7))
(define children (make-eq-hashtable))
(define parents (make-eq-hashtable))
(define weights (make-eq-hashtable))
(define nodes '())

(define (next-token line)
  (call-with-values
    (lambda () (read-token line))
    (lambda (type value start end) (cons type value))))

(define (parse line)
  (let ((t (next-token line)))
    (cond
      [(eq? (cdr t) #!eof) '()]
      [(eq? (car t) 'atomic) (cons (cdr t) (parse line))]
      [else (parse line)])))

(define (add-to-list-in-dict dict key value)
  (hashtable-set! dict key (cons value (hashtable-ref dict key '()))))

(do ((line (get-line input) (get-line input)))
  ((eq? #!eof line))
  (let ((parsed (parse (open-input-string line)) ))
    (set! nodes (cons (car parsed) nodes))
    (hashtable-set! weights (car parsed) (cadr parsed))
    (if (> (length parsed) 2)
      (for-each
        (lambda (x) 
          (begin
            (add-to-list-in-dict children (car parsed) x)
            (add-to-list-in-dict parents x (car parsed))))
        (cdddr parsed)))))

(for-each
  (lambda (n) 
    (if (eq? #f (hashtable-ref parents n #f))
      (set! root n)))
  nodes)
(write-part1 root)

(define (tower-weight top)
  (let ((w (hashtable-ref weights top #f)) (childlist (hashtable-ref children top '())))
    (if (null? childlist)
      w
      (+ w (apply + (map tower-weight childlist))))))

(define (occurences lst)
  (do ((occ (make-eq-hashtable) occ) (l lst (cdr l)))
    ((null? l) (map (lambda (k) (cons k (hashtable-ref occ k #f))) (vector->list (hashtable-keys occ))))
    (hashtable-set! occ (car l) (+ 1 (hashtable-ref occ (car l) 0)))))

;; returns pair of (odd-one-out, instance-of-rest) or #f if no unique element
(define (odd-one-out lst)
  (let ((sorted (sort (lambda (x y) (< (cdr x) (cdr y))) (occurences lst))))
    (if (= (cdar sorted) 1)
      (cons (car sorted) (cadr sorted))
      #f)))

(define (get-by-cdr v lst)
  (car (filter (lambda (x) (= v (cdr x))) lst)))

(define (p2 node diff)
  (let ((childlist (hashtable-ref children node '())))
    (let ((pairs (map (lambda (x) (cons x (tower-weight x))) childlist)))
      (let ((occ (odd-one-out (map cdr pairs))))
        (if (eq? #f occ)
          (- (hashtable-ref weights node #f) diff)
          (p2 (car (get-by-cdr (caar occ) pairs)) (abs (- (caar occ) (cadr occ))))
  )))))
(write-part2 (p2 root 0))
