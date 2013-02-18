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
