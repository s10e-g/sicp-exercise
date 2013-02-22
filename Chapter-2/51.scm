;;; ex-2.51
(load "common.scm")
(load "50.scm")

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-under
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point))
          (paint-over
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-under frame)
        (paint-over frame)))))

(define (below-b painter1 painter2)
  (minus270d (beside (minus270d (minus180d painter1))
                     (minus270d (minus180d painter2)))))
