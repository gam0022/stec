(define-struct tree (label children))

(define tree1 (make-tree 'a empty))
(define tree2 (make-tree 'b (list tree1)))
(define tree3 (make-tree 'c (list tree1 (make-tree 'd (list tree2)))))

; 葉であるか？
(define (tree-leaf? t) (empty? (tree-children t)))

; 大きさ
(define (tree-size-children c)
  (if
    (empty? c) 0
    (+ (tree-size (first c)) (tree-size-children (rest c)))))

(define (tree-size t)
  (if (tree-leaf? t) 1 (+ 1 (tree-size-children (tree-children t)))))

; 高さ
(define (tree-height-children c)
  (if
    (empty? c) 0
    (max (tree-height (first c)) (tree-height-children (rest c)))))

(define (tree-height t)
  (if (tree-leaf? t) 0 (+ 1 (tree-height-children (tree-children t)))))

; ラベルのリスト
(define (tree-list-children c)
  (if
    (empty? c) empty
    (append (tree-list (first c)) (tree-list-children (rest c)))))

(define (tree-list t)
  (cons (tree-label t) (tree-list-children (tree-children t))))