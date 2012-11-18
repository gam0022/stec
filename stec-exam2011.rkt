; 1
(define (pol2rect r theta)
  (list (* r (cos theta)) (* r (sin theta))))

; 2
(define (push l item) (append l (list item)))
(define (list-split l)
  (define (list-split-odd l odd even)
    (cond
      [(empty? l) (list odd even)]
      [else (list-split-even (rest l) (push odd (first l)) even)]))
  (define (list-split-even l odd even)
    (cond
      [(empty? l) (list odd even)]
      [else (list-split-odd (rest l) odd (push even (first l)))]))
  (list-split-odd l empty empty))

; 3
(define-struct pos (x y))
(define (distance p1 p2)
  (sqrt (+ (* (- (pos-x p1) (pos-x p2)) (- (pos-x p1) (pos-x p2))) 
           (* (- (pos-y p1) (pos-y p2)) (- (pos-y p1) (pos-y p2))))))

(define (perimeter plist)
  (define (perimeter-core p0 p1 d plist pf) 
    (cond 
      [(empty? plist) (+ d (distance p0 p1) (distance p1 pf))]
      [else (perimeter-core p1 (first plist) (+ d (distance p0 p1)) (rest plist) pf)]))
  (perimeter-core (first plist) (first (rest plist)) 0 (rest plist) (first plist)))

(define p0 (make-pos 0 0))
(define p1 (make-pos 1 0))
(define p2 (make-pos 1 1))
(define p3 (make-pos 0 1))

; 4
(define (include? a l) 
  (cond
    [(empty? l) false]
    [else (if (eq? a (first l)) true (include? a (rest l)))]))

(define (remove-dup l)
(define (remove-dup-core l ll)
  (cond
    [(empty? l) (reverse ll)]
    [(include? (first l) ll) (remove-dup-core (rest l) ll)]
    [else (remove-dup-core (rest l) (cons (first l) ll))]))
  (remove-dup-core l empty))

; 5
(define (list-deriv f)
  (define (list-deriv-core f l n)
    (cond
      [(empty? f) (reverse l)]
      [else (list-deriv-core (rest f) (cons (* (first f) n) l) (add1 n))]))
  (list-deriv-core (rest f) empty 1))

; 6
(define (list2fun f)
       (define (list2fun-core f x x2 ans)
         (cond
           [(empty? f) ans]
           [else (list2fun-core (rest f) x (* x2 x) (+ (* (first f) x2) ans))]))
       (lambda (x) (list2fun-core f x 1 0)))

; 7
(define-struct tree (label left right))

(define tree1 (make-tree 'b (make-tree 'c empty empty) (make-tree 'd empty empty)))
(define tree2 (make-tree 'e (make-tree 'f empty empty) empty))
(define tree (make-tree 'a tree1 tree2))

(define (tree-level lev t)
  (define (tree-level-core lev t n)
    (cond
      [(empty? t) empty]
      [(eqv? n lev) (list (tree-label t))]
      [else (append (tree-level-core lev (tree-left t) (add1 n))
                    (tree-level-core lev (tree-right t) (add1 n)))]))
  (tree-level-core lev t 0))