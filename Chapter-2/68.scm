;;; ex-2.68
(load "common.scm")
(load "67.scm")

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (if (leaf? tree)
    '()
    (if (memq sym (symbols (left-branch tree)))
      (cons 0 (encode-symbol sym (left-branch tree)))
      (if (memq sym (symbols (right-branch tree)))
        (cons 1 (encode-symbol sym (right-branch tree)))
        (error "can not find symbol")))))
