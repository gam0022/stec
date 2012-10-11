(define alist (list (list 'a 3) (list 'b 10) (list 'c true) (list 'd 0.3)))

(define (second_ x) (first (rest x)))

(define (alist-assoc key alist)
	(cond
		[(empty? alist) 'Not_found]
		[else 
			(cond	 [(eq? key (first (first alist))) (second_ (first alist))]
				 [else 
					(alist-assoc key (rest alist))])]))

(define (alist-mem key alist)
	(cond
		[(empty? alist) false]
		[else 
			(cond	 [(eq? key (first (first alist))) true]
				 [else 
					(alist-mem key (rest alist))])]))

(define (alist-remove key alist)
	(cond
		[(empty? alist) empty]
		[else 
			(cond	 [(eq? key (first (first alist))) (alist-remove key (rest alist))]
				 [else 
					(cons (first alist) (alist-remove key (rest alist)))])]))

(define (alist-add key val alist) (cons (list key val) alist))

(define (alist-keys alist)
  	(cond
		[(empty? alist) empty]
		[else 
			(cons (first (first alist)) (alist-keys (rest alist)))]))

(define (alist-values alist)
  	(cond
		[(empty? alist) empty]
		[else 
			(cons (second_ (first alist)) (alist-values (rest alist)))]))