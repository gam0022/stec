(define-struct expr (opc opr1 opr2))
(define (myeval e) 
  (cond
    [(eq? (expr-opc e) 'PLUS) (+ (expr-opr1 e) (expr-opr2 e))]
    [(eq? (expr-opc e) 'MUL) (* (expr-opr1 e) (expr-opr2 e))]
    [(eq? (expr-opc e) 'DIV) (/ (expr-opr1 e) (expr-opr2 e))]
    [(eq? (expr-opc e) 'SUB) (- (expr-opr1 e) (expr-opr2 e))]))
