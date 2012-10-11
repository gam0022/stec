(define (middle3 a b c) 
  (cond
    [(or (and (<= b a) (<= a c)) (and (>= b a) (>= a c))) a]
    [(or (and (<= a b) (<= b c)) (and (>= a b) (>= b c))) b]
    [(or (and (<= a c) (<= c b)) (and (>= a c) (>= c b))) c]))

(define (max2 a b) (if (>= a b) a b))

(define (max5 a b c d e) (max2 (max2 a b) (max2 (max2 c d) e)))
