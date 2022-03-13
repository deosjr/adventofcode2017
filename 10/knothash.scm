(define (next-token line)
  (call-with-values
    (lambda () (read-token line))
    (lambda (type value start end) value)))

;; comma-separated digits
(define (csds line)
  (let ((next (next-token line)))
    (cond
      [(eq? #!eof next) '()]
      [(eq? 'unquote next) (csds line)]
      [else (cons next (csds line))])))

(define (split-at n lst)
  (if (= n 0)
    (cons '() lst)
    (let ((prev (split-at (- n 1) (cdr lst))))
      (cons (cons (car lst) (car prev)) (cdr prev)))))

(define (original-order lst idxpointer)
  (let ((split (split-at (- (length lst) idxpointer) lst)))
    (append (cdr split) (car split))))

(define (hashround lst current skip lns)
  (let ((len (car lns)) (remlns (cdr lns)))
    (let ((split (split-at len lst)) (newcurrent (mod (+ current len skip) (length lst))))
      (let ((newsplit (split-at (mod (+ len skip) (length lst)) (append (reverse (car split)) (cdr split)))))
        (let ((rec (append (cdr newsplit) (car newsplit))))
          (if (null? remlns)
            (let ((last (original-order rec newcurrent)))
              (cons (* (car last) (cadr last)) (cons rec (cons newcurrent (+ skip 1)))))
            (hashround rec newcurrent (mod (+ skip 1) (length lst)) remlns)
          ))))))

(define (knothashkey k) (append (map char->integer (string->list k)) '(17 31 73 47 23)))

(define (densehash lst)
  (if (null? lst)
    '()
    (let ((split (split-at 16 lst)))
      (cons (apply bitwise-xor (car split)) (densehash (cdr split))))))

(define (knothashfunc lst current skip lns n)
  (if (= n 0)
    (densehash (original-order lst current))
    (let ((r (hashround lst current skip lns)))
      (let ((newlst (cadr r)) (newcurrent (caddr r)) (newskip (cdddr r)))
        (knothashfunc newlst newcurrent newskip lns (- n 1))))))

(define (knothash k) (knothashfunc (iota 256) 0 0 k 64))
