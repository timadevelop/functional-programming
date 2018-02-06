#lang scheme
(require rackunit rackunit/text-ui)

; Задача.
; Да се напише функция (meet-twice? f g a b), която проверява дали в
; целочисления интервал [a; b] съществуват две различни цели числа x и y
; такива, че f(x) = g(x) и f(y) = g(y).

(define (accumulate combiner null-value term a next b)
  (define (iter a last)
    (if (> a b) last
        (iter (next a) (combiner last (term a)))))
  (iter a null-value))


(define (meet-twice? f g a b)
  (define (p? x)
    (= (f x) (g x)))
   ; PROBLEM WITH DUPLICATE
  ;; (for-some (a b) p?)
  (> 
  (accumulate + 0
              (lambda (x) (if (p? x) 1 0))
              a
              (lambda(x) (+ 1 x))
              b) 1))



(define (identity x) x)
(define (negate x) (- x))


(define meet-twice?-tests
  (test-suite
   "Tests for meet-twice?"

   (check-false (meet-twice? identity negate 0 0))
   (check-false (meet-twice? identity negate -3 1))
   (check-true (meet-twice? identity sqrt 0 5))))

(run-tests meet-twice?-tests)