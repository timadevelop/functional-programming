(define (fact n) 

  (if (= n 0)
    1
    (* n (fact (- n 1)))
  )
)

(display 1233)

(display (fact 0))

(display (fact 1))

(display (fact 3))

(define (fib n)
	(define (iter i fi fi-1)
  		(if (= i n) fi (iter (+ i 1) (+ fi fi-1) fi)))
	(if (= n 0) 0 (iter 1 1 0))
)




(define (sum a b term next)
  (if (> a b)
    0
    (+ (term a) 
       (sum (next a) b term next)
    ))  
)
