;;; ex-2.54
(define (equal-b? a b)
  (if (and (pair? a) (pair? b))
    (and (equal-b? (car a) (car b)) (equal-b? (cdr a) (cdr b)))
    (if (or (pair? a) (pair? b))
      #f
      (eq? a b))))
