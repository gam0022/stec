;10-5
(define (max3chain l)
  (define (max3chain-core l a b c maxs)
    (cond
      [(empty? l) (cons (+ a b c) maxs)]
      [else (max3chain-core (rest l) b c (first l) (cons (+ a b c) maxs))]))
  (apply max(max3chain-core (rest(rest(rest l))) (first l) (first (rest l)) (first (rest(rest l))) empty)))