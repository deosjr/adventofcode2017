#! /usr/bin/scheme --script
(load "lib/lib.scm")
(load "10/knothash.scm")

(define lengthsp1 (csds (open-input-string (read-file-oneline 10))))

(write-part1 (car (hashround (iota 256) 0 0 lengthsp1)))

(define lengthsp2 (knothashkey (read-file-oneline 10)))

(printf "Part 2: ")
(for-each (lambda (x) (printf "~(~2,'0x~)" x)) (knothash lengthsp2))
(newline)
