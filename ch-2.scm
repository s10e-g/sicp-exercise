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


;;; 2.2.3
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))


;;; ex-2.33
(define (ya-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (ya-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (ya-length sequence)
  (accumulate (lambda (x y) (+ 1 y))0 sequence))


;;; ex-2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))


;;; ex-2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (sub-tree)
                         (if (pair? sub-tree)
                             (count-leaves sub-tree)
                             1))
                       t)))


;;; ex-2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


;;; ex-2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (vec) (dot-product v vec)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (vec) (matrix-*-vector cols vec)) m)))


;;; ex-2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter(op result (car rest))
             (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)


;;; ex-2.39
(define (reverse-1 sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-2 sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))


;;; ex-2.40
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


;;; ex-2.41
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


;;; ex-2.42
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


;;; ex-2.43
