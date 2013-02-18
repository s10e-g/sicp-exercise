;;; ex-2.20
(define (same-party first . l)
  (define (same-party-bk first l)
    (if (null? l)
      '()
      (if (even? (- first (car l)))
        (cons (car l) (same-party-bk first (cdr l)))
        (same-party-bk first (cdr l)))))
  (cons first (same-party-bk first l)))
