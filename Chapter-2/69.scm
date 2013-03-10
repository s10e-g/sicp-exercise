;;; ex-2.69
(load "common.scm")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
    (car leaf-set)
    (successive-merge
      (adjoin-leaf-set
        (make-code-tree (car leaf-set) (cadr leaf-set))
        (cddr leaf-set)))))
