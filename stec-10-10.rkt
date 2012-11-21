;10-10
(define (step x f i)
  (cond
    [(< i 0) empty]
    [(= x (f i)) i]
    [else (step x f (- i 1))]))

(define (invf f)
  (lambda(x) (step x f 10)))