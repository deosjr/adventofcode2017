#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define (solve file garbage groups score numgarbage)
  (let ((ch (read-char file)))
    (cond
      ((eq? ch #!eof) (cons score numgarbage))
      (else (if garbage
        (cond 
          [(eq? ch #\!) (read-char file) (solve file garbage groups score numgarbage)]
          [(eq? ch #\>) (solve file #f groups score numgarbage)]
          [else (solve file #t groups score (+ numgarbage 1))])
        (cond
          [(eq? ch #\{) (solve file garbage (+ groups 1) score numgarbage)]
          [(eq? ch #\}) (solve file garbage (- groups 1) (+ score groups) numgarbage)]
          [(eq? ch #\<) (solve file #t groups score numgarbage)]
          [(eq? ch #\,) (solve file garbage groups score numgarbage)]
          [(eq? ch #\newline) (solve file garbage groups score numgarbage)]))))))

(define ans (solve (filehandle 9) #f 0 0 0))
(write-part1 (car ans))
(write-part2 (cdr ans))
