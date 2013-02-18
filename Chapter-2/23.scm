;;; ex-2.23
(define (for-each f l)
  (if (not (null? l))
    (begin
      (f (car l))
      (for-each f (cdr l)))))
