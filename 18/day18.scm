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

; TODO: a string reader in lib that reads and stores in a list?
; smth like (str-reader line format) where format is a list like ('string 'char 'integer 'integer)
(define instructions (make-hashtable equal-hash equal?))
(define registers (make-hashtable equal-hash equal?))

;; each of these returns new (instr idx, return value)
;; ONLY RECOVER ACTUALLY RETURNS A VALUE (last stored sound)
(define (snd reg)
  (lambda ()
    (hashtable-set! registers 'sound (hashtable-ref registers reg 0))
    (cons 1 0)))

(define (set reg opt)
  (lambda ()
    (if (number? opt)
        (hashtable-set! registers reg opt)
        (hashtable-set! registers reg (hashtable-ref registers opt 0))
    )
    (cons 1 0)))

(define (add reg opt)
  (lambda ()
    (if (number? opt)
        (hashtable-set! registers reg (+ (hashtable-ref registers reg 0) opt))
        (hashtable-set! registers reg (+ (hashtable-ref registers reg 0) (hashtable-ref registers opt 0)))
    )
    (cons 1 0)))

(define (mul reg opt)
  (lambda ()
    (if (number? opt)
        (hashtable-set! registers reg (* (hashtable-ref registers reg 0) opt))
        (hashtable-set! registers reg (* (hashtable-ref registers reg 0) (hashtable-ref registers opt 0)))
    )
    (cons 1 0)))

(define (mod reg opt)
  (lambda ()
    (if (number? opt)
        (hashtable-set! registers reg (modulo (hashtable-ref registers reg 0) opt))
        (hashtable-set! registers reg (modulo (hashtable-ref registers reg 0) (hashtable-ref registers opt 0)))
    )
    (cons 1 0)))

(define (rcv reg)
  (lambda ()
    (if (eq? (hashtable-ref registers reg 0) 0)
      (cons 1 0)
      (cons 0 (hashtable-ref registers 'sound 0)))))

(define (jgz reg opt)
  (lambda ()
    (let ((jmp (hashtable-ref registers reg 0)))
      (if (eq? jmp 0)
        (cons 1 0)
        (if (number? opt)
          (cons opt 0)
          (cons (hashtable-ref registers opt 0) 0))))))

(define (parse-opt line)
  (let ((opt (substring line 6 (string-length line))))
    (let ((num (string->number opt)))
      (if (eq? num #f) opt num)
  )))

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
         (ret (f))
         (jmp (car ret))
         (snd (cdr ret)))
    (if (eq? snd 0)
      (part1 (+ idx jmp))
      snd
    )))

(write-part1 (part1 0))
