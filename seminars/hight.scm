(define (sum-int a b)
  (if (> a b)
    0
    (+ a 
       (sum-int (+ 1 a) b)
    )
  )
)
(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (* a a a)
       (sum-cubes (+ a 1) b)
    )
  )
)

(define (sum-series a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2) ) )
       (sum-series (+ a 3) b)
    )
  )
)


(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
    	(sum term (next a) next b)
    )
  )
)

(define (sum-int a b)
  	(define (identity x) x)
  	(define (inc x) (+ 1 x))
	(sum identity a inc b))

(define (sum-cubes a b)
  	(define (inc x) (+ 1 x))
  	(define (cube x) (* x x x))
	(sum cube a inc b))
