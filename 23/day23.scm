#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define (set x y)
  (let ((cast (string->number y)))
    (lambda (regs)
      (let ((value (if (number? cast) cast (hashtable-ref regs y 0))))
        (hashtable-set! regs x value) 1))))

(define (sub x y)
  (let ((cast (string->number y)))
    (lambda (regs)
      (let ((value (if (number? cast) cast (hashtable-ref regs y 0))))
        (hashtable-set! regs x (- (hashtable-ref regs x 0) value)) 1))))

(define (mul x y)
  (let ((cast (string->number y)))
    (lambda (regs)
      (set! sum (+ sum 1))
      (let ((value (if (number? cast) cast (hashtable-ref regs y 0))))
        (hashtable-set! regs x (* (hashtable-ref regs x 0) value)) 1))))

(define (jnz x y)
  (let ((castx (string->number x))
        (casty (string->number y)))
    (lambda (regs)
      (let ((valuex (if (number? castx) castx (hashtable-ref regs x 0)))
            (valuey (if (number? casty) casty (hashtable-ref regs y 0))))
        (if (eq? valuex 0) 1 valuey)))))

(define (parse-instr instr a b)
  (cond
    [(string=? instr "set") (set a b)]
    [(string=? instr "sub") (sub a b)]
    [(string=? instr "mul") (mul a b)]
    [(string=? instr "jnz") (jnz a b)]))

(define (read-input)
  (define m (make-hashtable equal-hash equal?))
  (define (readline input idx)
    (let ((line (get-line input)))
      (if (not (eq? line #!eof))
        (let-values (((instr a b) (sscanf line "%s %s %s")))
          (hashtable-set! m idx (parse-instr instr a b))
          (readline input (+ idx 1))))))
  (readline (filehandle 23) 0) m)

(define sum 0)

(define (part1 instructions registers idx) 
  (let ((f (hashtable-ref instructions idx #f)))
    (if f (part1 instructions registers (+ idx (f registers))))))
(write-part1 (part1 (read-input) (make-hashtable equal-hash equal?) 0))
(write-part1 sum)
