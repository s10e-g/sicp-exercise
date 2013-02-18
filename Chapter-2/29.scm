;;; ex-2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (if (null? (cdr mobile))
    '()
    (cadr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (if (null? (cdr branch))
    '()
    (cadr branch)))

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
    (total-weight (branch-structure branch))
    (branch-structure branch)))

(define (total-weight mobile)
  (if (null? (right-branch mobile))
    (total-weight (left-branch mobile))
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile)))))

(define (weighted-branch-weight branch)
  (if (pair? (branch-structure branch))
    (* (branch-length branch) (total-weight (branch-structure branch)))
    (* (branch-length branch) (branch-structure branch))))

(define (balanced? mobile)
  (if (pair? mobile)
    (if (= (weighted-branch-weight (left-branch mobile))
           (weighted-branch-weight (right-branch mobile)))
      (and (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))
      false)
    true))
