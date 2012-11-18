(define dx 0.00001)
 
;(load "stec-7-1.rkt")
(define (sigma a b f)
  (if (> a b) 0 (+ (f a) (sigma (+ a 1) b f))))

(define (deriv g) (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (integ g) 
  (lambda (x)
  (if (>= x 0)
      (sigma 0 (/ x dx) (lambda (i) (* (g (* i dx)) dx)))
      (sigma 0 (* (/ x dx) -1) (lambda (i) (* (g (* i (* dx -1))) (* dx -1)))))))

(define my-cos (deriv sin))
(define my-sin (integ cos))
(define my-cos2 (deriv my-sin))