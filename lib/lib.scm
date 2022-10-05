;; This was too annoying, but keeping it around in case I change my mind
;;(library (lib lib)
  ;;(export write-part1 write-part2 filehandle read-file-oneline read-entire-file)
  ;;(import (rnrs) (only (chezscheme) define printf))

(define (day n)
  (if (< n 10)
    (string-append "0" (number->string n))
    (number->string n)
  ))

(define (filehandle n) 
    (let ((filename (string-append (day n) (string-append "/day" (string-append (day n) ".input")))))
      (open-input-file filename)))

(define (read-file-oneline n)
    (get-line (filehandle n)))

(define (read-entire-file n)
    (get-string-all (filehandle n)))

(define (write-part1 ans) (printf "Part 1: ~w\n" ans))
(define (write-part2 ans) (printf "Part 2: ~w\n" ans))

; (sscanf "p=<1,2,3>" "p=<%d,%d,%d>") -> (1 2 3)
; silently fails and returns whatever succeeded so far in a list
(define (sscanf str fmt)
  (define strhandle (open-input-string str)) (define fmthandle (open-input-string fmt))
  ; parse a string until space or eof
  (define (scanstr)
    (define (scanstr-rec)
      (let ((next (peek-char strhandle)))
      (if (or (eq? next #\space) (eq? next #!eof)) '() (cons (read-char strhandle) (scanstr-rec)))))
    (list->string (scanstr-rec)))
  ; parse an integer
  (define (scanint)
    (define minus
      (if (eq? #\- (peek-char strhandle))
        (read-char strhandle) #f))
    (define (scanint-rec)
      (if (char-numeric? (peek-char strhandle)) (cons (read-char strhandle) (scanint-rec)) '()))
    (let ((digits (scanint-rec)))
      (if (not minus)
        (string->number (list->string digits))
        (string->number (list->string (cons minus digits))))))
  ; parse a single character
  (define (scanchar) (read-char strhandle))
  (define (sscanf-rec)
    (let ((c (read-char fmthandle)))
      (cond
        [(eq? c #!eof) '()]
        [(eq? c #\%)
         (let ((verb (read-char fmthandle)))
           (cond
             [(eq? verb #\d) (cons (scanint) (sscanf-rec))]
             [(eq? verb #\s) (cons (scanstr) (sscanf-rec))]
             [(eq? verb #\c) (cons (scanchar) (sscanf-rec))]
             [else '()]
         ))]
        [else (if (eq? c (read-char strhandle)) (sscanf-rec) '())])))
  (apply values (sscanf-rec)))
