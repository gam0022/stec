(define (product clist)
  (define (prec x clist)
    (if (empty? clist) x (prec (* x (first clist)) (rest clist))))
  (prec 1 clist))

(define (make-list n f)
  (define (mlrec l n)
  (if (= n 0) l (mlrec (cons (f (- n 1)) l) (- n 1))))
  (mlrec empty n))