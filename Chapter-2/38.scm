;;; ex-2.38
(load "common.scm")

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter(op result (car rest))
             (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)
