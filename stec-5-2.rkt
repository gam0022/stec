(define-struct child (father mother name date eyes))
(define Carl (make-child empty empty 'Carl 1926 'green))
(define Bettina (make-child empty empty 'Bettina 1926 'green))
(define Adam (make-child Carl Bettina 'Adam 1955 'black))
(define Dave (make-child Carl Bettina 'Dave 1950 'yellow))
(define Eva (make-child Carl Bettina 'Eva 1965 'blue))
(define Fred (make-child empty empty 'Fred 1938 'black))
(define Grace (make-child empty empty 'Grace 1940 'blue))
(define Harry (make-child Fred Grace 'Harry 1966 'blue))
(define Iwan (make-child Harry Eva 'Iwan 1987 'yellow))
(define Janet (make-child empty empty 'Jacob 1959 'blue))
(define Katie (make-child Adam Janet 'Katie 1985 'blue))

(define (brother? p1 p2) 
  (and
    (child? (child-father p1))
    (child? (child-mother p1))
    (eq? (child-father p1) (child-father p2))
    (eq? (child-mother p1) (child-mother p2))))

(define (child-gfathers p)
  (cond 
    [(and (child? (child-father(child-father p))) (child? (child-father(child-mother p))))
     (list (child-name(child-father(child-father p))) (child-name(child-father(child-mother p))))]
    [(child? (child-father(child-father p))) (list(child-name(child-father(child-father p))))]
    [(child? (child-father(child-mother p))) (list(child-name(child-father(child-mother p))))]
    [else empty]))

(define (ancestor p)
  (cond 
    [(and (child? (child-father p)) (child? (child-mother p)))
      (cons (child-name p) (append (ancestor(child-father p)) (ancestor(child-mother p))))]
    [(child? (child-father p)) (cons (child-name p) (ancestor(child-father p)))]
    [(child? (child-mother p)) (cons (child-name p) (ancestor(child-mother p)))]
    [else (list(child-name p))]))