(define (calc-time h m) 
  (* 60 (/ 5 4) (+ (* h (/ 5 8))
    (cond
      [(<= m 30) (/ m 60)]
      [(<= m 45) (+ (/ 1 2) (/ (- m 30) 120))]
      [(< 45 m) (/ 5 8)]))))