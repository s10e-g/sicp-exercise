;;; ex-2.27
(define (deep-reverse l)
  (define (deep-reverse-bk l pre)
    (if (null? (cdr l))
      (if (pair? (car l))
        (cons (deep-reverse-bk (car l) '()) pre)
        (cons (car l) pre))
      (if (pair? (car l))
        (deep-reverse-bk (cdr l) (cons (deep-reverse-bk (car l) '()) pre))
        (deep-reverse-bk (cdr l) (cons (car l) pre)))))
  (deep-reverse-bk l '()))
