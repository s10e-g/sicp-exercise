;;; ex-2.80
(load "common.scm")

(define (=zero? x y)
  (apply-generic '=zero? x y))

(define (install-=zero?-package)
  (put '=zero? 'scheme-number
       (lambda (x)
         (= x 0)))
  (put '=zero? 'rational
       (lambda (x)
         (= (numer x) 0)))
  (put '=zero? 'complex
       (lambda (x)
         (or (and (= (real-part x) 0)
                  (= (imag-part x) 0)
             (and (= (magnitude x) 0)
                  (= (angle x) 0))))))
  'done)
