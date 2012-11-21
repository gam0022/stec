;10-4
(define-struct wp (header body))

(define wp3 (make-wp 'wp3 (list 'hello 'world)))
(define wp2 (make-wp 'wp2 (list 'violence 'dangerous)))
(define wp1 (make-wp 'wp1 (list 'no 'problem wp2 wp3)))

(define (browze-wp w)
  (cons (wp-header w) (list (map (lambda (w) (if (wp? w) (browze-wp w) w)) (wp-body w)))))

(define black-list (list 'violence 'terrorist))

(define (include? a l)
  (cond
    [(empty? l) false]
    [(eqv? a (first l)) true]
    [else (include? a (rest l))]))

(define (remove-empty l)
  (cond
    [(empty? l) empty]
    [(empty? (first l)) (rest l)]
    [else (cons (first l) (rest l))]))

(define (filter-wp bl w)
  (make-wp (wp-header w) 
    (remove-empty (map (lambda (w) 
      (cond
        [(wp? w) (filter-wp bl w)]
        [(include? w bl) empty]
        [else w])) (wp-body w)))))

(define (replace-wp w s t)
  (make-wp (wp-header w) 
    (map (lambda (w) 
      (cond
        [(wp? w) (replace-wp w s t)]
        [(eqv? w s) t]
        [else w])) (wp-body w))))