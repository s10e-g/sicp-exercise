;;; ex-2.28
(define (fringe tr)
  (if (null? tr)
    '()
    (if (pair? tr)
      (if (pair? (car tr))
        (append (fringe (car tr)) (fringe(cdr tr)))
        (cons (fringe (car tr)) (fringe(cdr tr))))
      tr)))
