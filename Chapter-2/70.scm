;;; ex-2.70
(load "68.scm")
(load "69.scm")

(define pair_70 '((BOOM 1) (WAH 1) (A 2) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16)))

(define msg_70 '(Get a job
                     Sha na na na na na na na na
                     Get a job
                     Sha na na na na na na na na
                     Wah yip yip yip yip yip yip yip yip yip
                     Sha boom))

(define tree_70 (generate-huffman-tree pair_70))

(define res_70 (encode msg_70 tree_70))
