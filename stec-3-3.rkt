(define-struct point (x y z))
(define-struct cube (c r))
(define (_over p1 p2 r) (<= (abs (- p1 p2)) r))
(define (over c1 c2) 
  (define r (* 0.5 (+ (cube-r c1) (cube-r c2))))
  (and 
    (_over (point-x(cube-c c1)) (point-x(cube-c c2)) r) 
    (_over (point-y(cube-c c1)) (point-y(cube-c c2)) r) 
    (_over (point-z(cube-c c1)) (point-z(cube-c c2)) r)))
