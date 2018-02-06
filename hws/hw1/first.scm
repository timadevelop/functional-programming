
(define (valid? function args)
  (and (procedure? function) (list? args)) 
)

(define (find op function list max)
  (if (null? list)
      max
      (if (op (function (car list)) (function max)) ; if current element > last max ;; >= for outputing last of eq
          (find op function (cdr list) (car list)) ; current element is max
          (find op function (cdr list) max) ; last max element is max
      )
  )
)

(define (argmax function args)
	(if (valid? function args)
            (find > function args (car args))
            (error '"bad input")
	)
)

(define (argmin function args)
	(if (valid? function args)
            (find < function args (car args))
            (error '"bad input")
	)
)

(print (argmax (lambda (x) (* x x)) '(1 3 0 4 2.5 -4))) ; -> 4 ; but not 4
(print (argmin length '((1 2) () (2 a 5 7) (2 4)))) ; -> '()
