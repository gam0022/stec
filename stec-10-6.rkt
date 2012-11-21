;10-6
(define-struct pos (x y))
(define (distance p1 p2)
  (sqrt (+ (expt (- (pos-x p1) (pos-x p2)) 2)
           (expt (- (pos-y p1) (pos-y p2)) 2))))

(define (area-s pa pb pc)
  (/ (+ (distance pa pb) (distance pb pc) (distance pa pc) ) 2))

(define (area pa pb pc)
  (define s (area-s pa pb pc))
  (sqrt (* s (- s (distance pa pb)) (- s (distance pb pc)) (- s (distance pa pc)))))

(define (third_ l) (first(rest(rest l))))

(define (drop l n)
  (if (eqv? n 0) l (drop (rest l) (- n 1))))

(define (pol-area l)
  (define (pol-area-core l pa pb p1 s)
    (cond
      [(empty? l) (+ (area p1 pa pb) s)]
      [else (pol-area-core (rest l) pb (first l) p1 (+ (area p1 pa pb) s))]))
  (pol-area-core (drop l 3) (second l) (third_ l) (first l) 0))

(define p1 (make-pos 0 0))
(define p2 (make-pos 1 0))
(define p3 (make-pos 2 1))
(define p4 (make-pos 1 2))
(define p5 (make-pos 0 1))
(pol-area (list p1 p2 p3 p4 p5))
