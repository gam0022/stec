(define-struct z ())
(define-struct s (p))

(define (plus r1 r2) 
  (cond [(z? r2) r1]
        [else (make-s (plus r1 (s-p r2)))]))

(define (sub r1 r2) 
  (cond [(z? r2) r1]
        [else (s-p (sub r1 (s-p r2)))]))

(define (to-racket n) 
  (cond [(zero? n) (make-z)]
         [else (make-s (to-racket(- n 1)))]))

(define (to-num rkt)
  (cond [(z? rkt) 0]
        [else (+ 1 (to-num (s-p rkt)))]))