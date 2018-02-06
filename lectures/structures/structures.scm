(define (count-leaves l)
  (cond
    ((null? l) 0)
    ((pair? l) (+ (count-leaves (car l)) (count-leaves (cdr l))))
	    (else 1)
  )
)

(print (count-leaves (cons (list 1 2) (list 3 4))))

(define (reverse1 l)
  (cond ((null? l) null)
	((
)

(print (count-leaves (cons (list 1 2) (list 3 4))))


;;(print (count-leaves (cons (list 1 2) (list 3 4))))
