;;; ex-2.75
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          ((eq? op 'real-part)
           (* mag (cos ang)))
          ((eq? op 'imag-part)
           (* mag (sin ang)))
          (else
            (error "Unknown op -- MAKE_FROM_REAL_IMAG" op))))
  dispatch)
