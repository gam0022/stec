; 正四面体の体積
(define (volume a) (* (/ (sqrt 2) 12) (expt a 3)))
; 常用対数
(define (clog a) (/ (log a) (log 10)))
; f(x) = x3 + 4x2 - 8x - 3
(define (f x) (- (- (+ (expt x 3) (* 4 (expt x 2))) (* 8 x)) 3))