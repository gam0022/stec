(define-struct add (left right))
(define-struct sub (left right))
(define-struct mul (left right))
(define-struct div (left right))
(define-struct num (val))
(define-struct var (name))

(define (second_ x) (first (rest x)))

(define (alist-assoc key alist)
	(cond
		[(empty? alist) 0]
		[else 
			(cond	 [(eq? key (first (first alist))) (second_ (first alist))]
				 [else 
					(alist-assoc key (rest alist))])]))

(define env (list (list 'a 1) (list 'b 2) (list 'c 3)))

(define (myeval e f)
  (cond 
    [(add? f) (+ (myeval e (add-left f)) (myeval e (add-right f)))]
    [(sub? f) (- (myeval e (sub-left f)) (myeval e (sub-right f)))]
    [(mul? f) (* (myeval e (mul-left f)) (myeval e (mul-right f)))]
    [(div? f) (if (= (myeval e (div-right f)) 0)
                  0
                  (/ (myeval e (div-left f)) (myeval e (div-right f))))]
    [(num? f) (num-val f)]
    [(var? f) (alist-assoc (var-name f) e)]))