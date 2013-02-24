;;; ex-2.57
(load "common.scm")

(define (augend s)
  (if (null? (cdddr s))
    (caddr s)
    (make-sum (caddr s) (cadddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
    (caddr p)
    (make-product (caddr p) (cadddr p))))
