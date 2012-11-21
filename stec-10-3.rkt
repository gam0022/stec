;10-3
;(define (sqrlist l) (map (lambda(x) (expt x 2)) l))
(define (sqrlist l)
  (cond
    [(empty? l) empty]
    [(>= (first l) 0) (cons (expt (first l) 2) (sqrlist (rest l)))]
    [else (sqrlist (rest l))]))