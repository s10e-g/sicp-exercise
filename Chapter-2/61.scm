;;; ex-2.61
(load "common.scm")

(define (adjoin-sorted-set x set)
  (let ((y (car set)))
    (cond ((= x y) set)
          ((< x y) (cons x set))
          ((> x y) (cons y (adjoin-sorted-set x (cdr set)))))))
