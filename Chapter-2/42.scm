;;; ex-2.42
(load "common.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (safe? k positions)
  (define (check new-row rest-of-queens d)
    (if (null? rest-of-queens)
      #t
      (if (or (= new-row (car rest-of-queens))
              (= new-row (- (car rest-of-queens) d))
              (= new-row (+ (car rest-of-queens) d)))
        #f
        (check new-row (cdr rest-of-queens) (+ d 1)))))
  (check (car positions) (cdr positions) 1))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))
