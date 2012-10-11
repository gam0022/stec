(define (area shape a b)
  (cond
    [(or (< a 0) (< b 0)) 'Undefined]
    [(eq? shape 'TRIANGLE) (* 0.5 a b)]
    [(eq? shape 'RECTANGLE) (* a b)]
    [(eq? shape 'CYLINDER) (+ (* 2 pi a a) (* 2 pi a b))]
    [(eq? shape 'SECTOR)
      (if (> b 360) 'Undefined (* pi a a (/ b 360)) )]))
