;;; ex-2.18
(define (reverse l)
  (define (reverse-bk l pre)
    (if (null? (cdr l))
      (cons (car l) pre)
      (reverse-bk (cdr l) (cons (car l) pre))))
  (reverse-bk l '()))
