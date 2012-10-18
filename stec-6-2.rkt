(define-struct wp (header body))

(define wp3 (make-wp 'wp3 (list 'hello 'world)))
(define wp2 (make-wp 'wp2 (list 'violence 'dangerous)))
(define wp1 (make-wp 'wp1 (list 'no 'problem wp2 wp3)))

; Webページを閲覧する
(define (browze-wp-body b)
  (cond
    [(empty? b) empty]
    [(wp? (first b)) (cons (browze-wp (first b)) (browze-wp-body (rest b)))]
    [else (cons (first b) (browze-wp-body (rest b)))]))

(define (browze-wp w) 
  (if (empty? w) empty
    (list (wp-header w) (browze-wp-body (wp-body w)))))

(define black-list (list 'violence 'terrorist))

; lにiが含まれていたら真
(define (include? i l)
  (cond
    [(empty? l) false]
    [(eq? i (first l)) true]
    [else (include? i (rest l))]))

; Webページのフィルタリングを行う
(define (filter-wp-body a bl)
  (cond
    [(empty? a) empty]
    [(wp? (first a)) (cons (filter-wp bl (first a)) (filter-wp-body (rest a) bl))]
    [(include? (first a) bl) (filter-wp-body (rest a) bl)]
    [else (cons (first a) (filter-wp-body (rest a) bl))]))

(define (filter-wp bl w)
  (if (empty? w) empty
     (make-wp (wp-header w) (filter-wp-body (wp-body w) bl))))