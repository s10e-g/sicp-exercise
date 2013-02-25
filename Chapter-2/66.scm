;;; ex-2.66
(define (lookup given-key BST-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry BST-of-records)))
         (entry BST-of-records))
        ((< given-key (key (entry BST-of-records)))
         (lookup given-key (left-branch BST-of-records)))
        ((> given-key (key (entry BST-of-records)))
         (lookup given-key (right-branch BST-of-records)))))
