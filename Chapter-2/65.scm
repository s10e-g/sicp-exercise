;;; ex-2.65
(load "common.scm")
(load "62.scm")
(load "63.scm")
(load "64.scm")

(define (union-BST tree1 tree2)
  (list->tree (union-sorted-set
                (tree->list-2 tree1)
                (tree->list-2 tree2))))

(define (intersection-BST tree1 tree2)
  (list->tree (intersection-sorted-set
                (tree->list-2 tree1)
                (tree->list-2 tree2))))
