#lang scheme
(define (1+ x) (+ x 1))
(define (accumulate combiner null-value term a next b)
  (define (iter a last)
    (if (> a b) last
        (iter (next a) (combiner (term a) last))))
  (iter a null-value))

(define (meetTwice? f g a b)
  (define (p? x)
    (= (f x) (g x)))
  (> (accumulate + 0
              (lambda(x) (if (p?  x) 1 0))
              a 1+ b)
  1))

(meetTwice? (lambda(x) x) (lambda(x) (- x)) -3 1)
(meetTwice?  (lambda(x)x) sqrt 0 5)
              