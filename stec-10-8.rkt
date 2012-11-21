;10-8
(define-struct tree (label left right))

(define (tree-height t)
  (cond
    [(empty? t) 0]
    [else (+ 1 (max (tree-height (tree-left t)) (tree-height (tree-right t))))]))

(define (balance-factor t)
  (abs (- (tree-height (tree-left t)) (tree-height (tree-right t)))))