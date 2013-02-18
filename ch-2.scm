;;;;    Exercises in Chapter 2

;;; ex-2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))


;;; ex-2.17
(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))


;;; ex-2.18
(define (reverse l)
  (define (reverse-bk l pre)
    (if (null? (cdr l))
      (cons (car l) pre)
      (reverse-bk (cdr l) (cons (car l) pre))))
  (reverse-bk l '()))


;;; ex-2.19
(define (cc amount coin-values)
  (define (first-denomination coin-values)
    (car coin-values))
  (define (except-first-denomination coin-values)
    (cdr coin-values))
  (define (no-more? coin-values)
    (null? coin-values))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values)) 
                 coin-values)))))


;;; ex-2.20
(define (same-party first . l)
  (define (same-party-bk first l)
    (if (null? l)
      '()
      (if (even? (- first (car l)))
        (cons (car l) (same-party-bk first (cdr l)))
        (same-party-bk first (cdr l)))))
  (cons first (same-party-bk first l)))


;;; ex-2.21
(define (square-list1 items)
  (if (null? items)
    '()
    (cons (* (car items) (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda x (* x x)) items))


;;; ex-2.23
(define (for-each f l)
  (if (not (null? l))
    (begin
      (f (car l))
      (for-each f (cdr l)))))


;;; ex-2.27
(define (deep-reverse l)
  (define (deep-reverse-bk l pre)
    (if (null? (cdr l))
      (if (pair? (car l))
        (cons (deep-reverse-bk (car l) '()) pre)
        (cons (car l) pre))
      (if (pair? (car l))
        (deep-reverse-bk (cdr l) (cons (deep-reverse-bk (car l) '()) pre))
        (deep-reverse-bk (cdr l) (cons (car l) pre)))))
  (deep-reverse-bk l '()))


;;; ex-2.28
(define (fringe tr)
  (if (null? tr)
    '()
    (if (pair? tr)
      (if (pair? (car tr))
        (append (fringe (car tr)) (fringe(cdr tr)))
        (cons (fringe (car tr)) (fringe(cdr tr))))
      tr)))


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


;;; ex-2.30
(define (square-tree1 tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (square-tree1 (car tree))
                            (square-tree1 (cdr tree))))
        (else (* tree tree))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree2 sub-tree)
           (* sub-tree sub-tree)))
       tree))


;;; ex-2.31
(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map func sub-tree)
           (func sub-tree)))
       tree))

(define (square a)
  (* a a))


;;; ex-2.32
(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x)
                          (cons (car s) x))
                        rest)))))
