;;; ex-2.31
(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map func sub-tree)
           (func sub-tree)))
       tree))

(define (square a)
  (* a a))
