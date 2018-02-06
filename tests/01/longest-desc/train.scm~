#lang scheme
(require rackunit rackunit/text-ui)
;;
;; ( (1 2 3) (4 5 6) (7 8 9) )

;; 
; Задача.
; Да се напише функция (check-matrix? m k), която проверява дали на всеки ред
; в дадена матрица m от цели числа има поне по едно число, кратно на k.
(define (foldr f null-value l)
  (if (null? l) null-value
      (f (car l) (foldr f null-value (cdr l)))))

(define (for-every? p? l)
  (foldr (lambda(x last) (and (p? x) last)) #t l))

(define (for-some? p? l)
  (foldr (lambda(x last) (or (p? x) last)) #f l))

(define (check-matrix? m k)
  (define (p? x)
    (= 0 (remainder x k)))
  ; (for-every? (row in m) (lambda(row) (for-some? row p?))
  (for-every? (lambda(row) (for-some? p? row)) m))

  
(define check-matrix?-tests
  (test-suite
   "Tests for check-matrix?"

   (check-true (check-matrix? '((1 2 6) (3 8 9) (10 12 11)) 3))
   (check-false (check-matrix? '((1 2 4) (3 8 9) (10 12 11)) 3))))

(run-tests check-matrix?-tests)