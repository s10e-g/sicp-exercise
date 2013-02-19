;;; ex-2.45
(define (split fir sec)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split fir sec) painter (- n 1))))
        (fir painter (sec smaller smaller))))))
