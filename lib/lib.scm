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
