;;; ex-2.41
(load "common.scm")

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 0 (- j 1))))
                      (enumerate-interval 0 (- i 1))))
           (enumerate-interval 0 n)))

(define (sum-s s)
  (filter (lambda (seq) (= s (+ (car seq) (cadr seq) (caddr seq))))
          (unique-triples (- s 1))))
