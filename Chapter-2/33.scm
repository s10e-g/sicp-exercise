;;; ex-2.33
(load "common.scm")

(define (ya-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (ya-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (ya-length sequence)
  (accumulate (lambda (x y) (+ 1 y))0 sequence))
