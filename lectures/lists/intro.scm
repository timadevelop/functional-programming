(define (list? l)
  (or (null? l) (and (pair? l) (list? (cdr l))))
)

; (list 1 2 3) <=> (cons 1 (cons 2 (cons 3 null)))
