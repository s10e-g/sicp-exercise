;;; ex-2.46
(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

(define (add-vect a b)
  (make-vect (+ (xcor-vect a) (xcor-vect b))
             (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a) (xcor-vect b))
             (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect a n)
  (make-vect (* (xcor-vect a) n)
             (* (ycor-vect a) n)))
