(define (list-replace symbols a b)
	(cond
		[(empty? symbols) empty]
		[else 
			(cond	 [(equal? (first symbols) a) (cons b (list-replace (rest symbols) a b))]
				 [else 
					(cons (first symbols) (list-replace (rest symbols) a b))])]))