(define (palindrome l)
  (define (worker l1 acc)
    (cond
      [(empty? (rest l1)) (append l acc)]
      [else (worker (rest l1) (cons (first l1) acc))]))
  (worker l empty)
)

; 問題の要件をみたすだけなら、以下でもOKな気がする
; (define (palindrome l) (append l (rest (reverse l))))