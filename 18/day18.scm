#! /usr/bin/scheme --script
(load "lib/lib.scm")

; move to lib: do smth line by line on input file
; continue updates accumulator, end returns result
; TODO: is this just a cumbersome do-loop?
(define (stream-handle-input file continue end acc)
  (let ((line (get-line file)))
    (if (eq? #!eof line)
      (end acc)
      (stream-handle-input file continue end (continue line acc))
    )))

(define instructions (make-hashtable equal-hash equal?))
(define registers (make-hashtable equal-hash equal?))

;; each of these returns new (instr idx, return value)
;; ONLY RECOVER ACTUALLY RETURNS A VALUE (last stored sound)
(define (snd reg)
  (lambda (regs)
    (hashtable-set! regs 'sound (hashtable-ref regs reg 0))
    (cons 1 #f)))

(define (set reg opt)
  (lambda (regs)
    (let ((value (if (number? opt) opt (hashtable-ref regs opt 0))))
      (hashtable-set! regs reg value))
    (cons 1 #f)))

(define (add reg opt)
  (lambda (regs)
    (let ((value (if (number? opt) opt (hashtable-ref regs opt 0))))
      (hashtable-set! regs reg (+ (hashtable-ref regs reg 0) value)))
    (cons 1 #f)))

(define (mul reg opt)
  (lambda (regs)
    (let ((value (if (number? opt) opt (hashtable-ref regs opt 0))))
      (hashtable-set! regs reg (* (hashtable-ref regs reg 0) value)))
    (cons 1 #f)))

(define (mod reg opt)
  (lambda (regs)
    (let ((value (if (number? opt) opt (hashtable-ref regs opt 0))))
      (hashtable-set! regs reg (modulo (hashtable-ref regs reg 0) value)))
    (cons 1 #f)))

(define (rcv reg)
  (lambda (regs)
    (if (eq? (hashtable-ref regs reg 0) 0)
      (cons 1 #f)
      (cons 0 (hashtable-ref regs 'sound 0)))))

(define (jgz reg opt)
  (lambda (regs)
    (let* ((num (string->number reg))
           (jmp (if (eq? num #f) (hashtable-ref regs reg 0) num)))
      (if (> jmp 0)
        (let ((value (if (number? opt) opt (hashtable-ref regs opt 0))))
          (cons value #f))
        (cons 1 #f)))))

(define (parse-opt line)
  (let ((opt (substring line 6 (string-length line))))
    (let ((num (string->number opt)))
      (if (eq? num #f) opt num)
  )))

; TODO: a string reader in lib that reads and stores in a list?
; smth like (str-reader line format) where format is a list like ('string 'char 'integer 'integer)
(define (parse-instructions file)
  (define (continue line acc)
    (let ((ins (substring line 0 3)) (reg (substring line 4 5)))
      (cond
        [(string=? ins "snd") (hashtable-set! instructions acc (snd reg))]
        [(string=? ins "set") (hashtable-set! instructions acc (set reg (parse-opt line)))]
        [(string=? ins "add") (hashtable-set! instructions acc (add reg (parse-opt line)))]
        [(string=? ins "mul") (hashtable-set! instructions acc (mul reg (parse-opt line)))]
        [(string=? ins "mod") (hashtable-set! instructions acc (mod reg (parse-opt line)))]
        [(string=? ins "rcv") (hashtable-set! instructions acc (rcv reg))]
        [(string=? ins "jgz") (hashtable-set! instructions acc (jgz reg (parse-opt line)))]
      )
    )
    (+ acc 1))
  (define (end acc) acc)
  (stream-handle-input file continue end 0))

(parse-instructions (filehandle 18))

(define (part1 idx) 
  (let* ((f (hashtable-ref instructions idx #f))
         (ret (f registers))
         (jmp (car ret))
         (snd (cdr ret)))
    (if snd snd (part1 (+ idx jmp)))))

(write-part1 (part1 0))

; clear out the instructions cause we are going to run again for p2
(define instructions (make-hashtable equal-hash equal?))
; instead of registers we have 2 programs, which include registers with string keys
(define program0 (make-hashtable equal-hash equal?))
(define program1 (make-hashtable equal-hash equal?))
(hashtable-set! program0 "p" 0)
(hashtable-set! program1 "p" 1)
(hashtable-set! program0 'id 0)
(hashtable-set! program1 'id 1)

(define queue '())

; redefine snd and receive
(define (snd reg)
  (lambda (regs)
    (let ((value (hashtable-ref regs reg 0)))
      (if (eq? 0 (hashtable-ref regs 'id #f))
        ; if this is program0, we can now wakeup program1 and continue there
        (cons 1 value)
        ; if this is program1, we send into a queue that program0 reads from and continue
        (begin
          (hashtable-set! regs 'sends (+ 1 (hashtable-ref regs 'sends 0)))
          (set! queue (cons value queue))
          (cons 1 #f))))))

(define (rcv reg)
  (lambda (regs)
    (if (eq? 0 (hashtable-ref regs 'id #f))
      ; if this is program0 and we dont have values, we have reached deadlock
      (if (null? queue)
        (cons 0 'deadlock)
        (begin
          (let ((rev (reverse queue)))
          (hashtable-set! regs reg (car rev))
          (set! queue (reverse (cdr rev)))
          (cons 1 #f))))
      ; if this is program1 we switch to program0 until we get a value back
      (let* ((msg (part2 program0))
             (received (cdr msg)))
        (if (eq? received 'deadlock)
          msg
          (begin
            (hashtable-set! regs reg received)
            (cons 1 #f)))))))

; we only care about program1 ending (it cannot send more after)
; we can run program0 only so far as it sends back a value to program1 each time
; this means we only run program0 if program1 is waiting
(define (part2 program) 
  (let* ((idx (hashtable-ref program 'idx 0))
         (f (hashtable-ref instructions idx #f)))
    (if (not f)
      (cons 0 'deadlock) ; either normal end or deadlock, but we dont care about the difference
      (let* ((ret (f program))
             (jmp (car ret))
             (snd (cdr ret)))
        (hashtable-set! program 'idx (+ idx jmp))
        (if snd ret (part2 program))))))

(parse-instructions (filehandle 18))
(part2 program1)
(write-part2 (hashtable-ref program1 'sends #f))
