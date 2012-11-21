;10-9
(define (embed? x y)
  (cond
    [(empty? x) true]
    [(empty? y) false]
    [(= (first x) (first y)) (embed? (rest x) (rest y))]
    [else (embed? x (rest y))]))