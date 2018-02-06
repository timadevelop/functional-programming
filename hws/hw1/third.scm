;;#lang scheme ;; for running in drracket
;;
;; Function for tests
;;
(define (print-result f) 
  (display "Using ")
  (display f)
  (display "\n")
  (print (f (lambda (x) (- (exp x) (* 3 x))) 0 1 0.001)) ; 0.6191
  (display "\n")
  (print (f (lambda (x) (expt (- x 2) 3)) 0 5 0.005)) ; 2
  (display "\n")
  (print (f (lambda (x) (- (sqrt x) (* 2 (log x)))) 1 5 0.001))  ; 2.044
  (display "\n")
  (print (f (lambda (x) (- (exp (sqrt x)) (* 2 x))) 5 10 0.001)) ; 6.853
  (display "\n")
)

;;
;; Finding root using binary search
;;
;;
(define (find-root f a b eps)
  (define (approx-zero? x)
    (< (abs x) eps)
  )

  (define (same-sign? x y)
    (define (normalize x) (/ x (abs x)))
      (= (normalize x) (normalize y))
  )

  (define (helper a b iter)
    (let ((mid (/ (+ a b) 2) ))
      (if (approx-zero? (f mid))
	(cons (real->decimal-string mid) iter)
	(cond
	  ( (same-sign? (f mid) (f a)) (helper mid b (+ 1 iter)))
	  ( (same-sign? (f mid) (f b)) (helper a mid (+ 1 iter)))
	))))
  (helper a b 0)
)

;; Some test outputs
(print-result find-root)

;;
;; Finding root using second method (with secant)
;;

(define (find-root-secant f a b eps)
  (define (next pre-last last)
    (- last (* (f last ) (/ (- last pre-last) (- (f last) (f pre-last)))))
  )
  (define (good-enough? x last)
    (< (abs (- x last)) eps)
  )
  (define (helper pre-last last iter)
    (if (good-enough? last pre-last)
    	(cons (real->decimal-string last) iter)
	(helper last (next pre-last last) (+ 1 iter))
    )
  )
  (helper a b 0)
)

;; Some test outputs
(print-result find-root-secant)

;;
;; Finding root using third approach (newton)
;;
(define vlad-made-third-function? #f) ;; I dont know if I`ll have time
#|
(define (find-root-newton f a b eps)
  (define (next last)
    (- last (/ (f last) ((derivate f) last)))

  )
  (define (good-enough? x last)
    (< (abs (- x last)) eps)
  )
  (define (helper pre-last last)
    (if (good-enough? last pre-last)
    	last
	(helper last (next pre-last last))
    )
  )
  (helper a b)
)
(define vlad-made-third-function? #t)
|#

;;
;; Bonus (compairing)
;;

(define (compare-methods f a b eps)
  (if vlad-made-third-function?
    (list (cdr (find-root f a b eps)) (cdr (find-root-secant f a b eps)) (cdr (find-root-newton f a b eps)))
    (list (cdr (find-root f a b eps)) (cdr (find-root-secant f a b eps)))
  ) 
)

(print-result compare-methods)
