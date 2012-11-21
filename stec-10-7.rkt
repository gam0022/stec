;10-7
(define-struct tree (label left right))

(define (sumtree t)
  (cond
    [(empty? t) 0]
    [else (+ (tree-label t) (sumtree (tree-left t)) (sumtree (tree-right t)))]))