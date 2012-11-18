(define (sigma a b f)
  (if (> a b) 0 (+ (f a) (sigma (+ a 1) b f))))

(define (find pred clist)
  (cond 
    [(empty? clist) 'Not_found]
    [(pred (first clist)) (first clist)]
    [else (find pred (rest clist))]))

(define (for-all? pred clist)
  (cond
    [(empty? clist) true]
    [(not (pred (first clist))) false]
    [else (for-all? pred (rest clist))]))

(define (exist? pred clist)
  (cond
    [(empty? clist) false]
    [(pred (first clist)) true]
    [else (exist? pred (rest clist))]))

(define (partition pred clist)
  (define (select pred clist)
    (cond
      [(empty? clist) empty]
      [(pred (first clist)) (cons (first clist) (select pred (rest clist)))]
      [else (select pred (rest clist))]))
  (define (reject pred clist)
    (cond
      [(empty? clist) empty]
      [(not (pred (first clist))) (cons (first clist) (reject pred (rest clist)))]
      [else (reject pred (rest clist))]))
  (list (select pred clist) (reject pred clist)))

(define (make-list n f)
  (if (< n 1) empty (append (make-list (- n 1) f) (list(f (- n 1))))))