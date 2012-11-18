;(define (fib n)
;  (cond [(= n 1) 1]
;        [(= n 2) 1]
;        [else
;          (+ (fib (- n 1)) (fib (- n 2)))]))

(define (fib n)
  (define (fib-iter n1 n2 count)
    (cond 
      [(= n count) (+ n1 n2)]
      [else (fib-iter n2 (+ n1 n2) (add1 count))]))
  (cond [(= n 1) 1]
        [(= n 2) 1]
        [else
          (fib-iter 1 1 3)]))