(define (list-sum x)
  (cond 
    [(empty? x)  0]
    [else (+ (first x) (list-sum (rest x)))]))
