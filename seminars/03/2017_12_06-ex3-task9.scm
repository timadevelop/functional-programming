#lang racket

;; task 9
;; Напишете функция double, която приема функция на
;;един аргумент и връща функция, която прилага подадената
;;функция два пъти. Например, ако имаме функцията inc,
;;която добавя 1 към своя аргумент ((define (inc x) (+ x 1)),
;;то (double inc) е функция, която добавя 2 към своя аргумент.
(define (inc x) (+ 1 x))
(define (double f)
  (lambda (x) (f (f x))))
((double inc) 2)

;; task 10
;;Нека f и g са функции на един аргумент.
;;Композицията f ∘ g е функцията x ↦ f(g(x)).
;;Напишете функция compose(f, g), която връща композицията f ∘ g.
(define (compose f g)
  (lambda(x) (f (g x))))
;;Пример:
;(define (inc x) (+ x 1))
(define (square x) (* x x))

((compose square inc) 6) ; 49

;; task 11
;;Нека f е функция на един аргумент и
;;n е цяло неотрицателно число.
;;Дефинираме n-тото прилагане на функцията f да бъде функцията,
;;дефинирана по следния начин: f0(x) = x;;;fn(x) = f(fn-1(x))

;;Напишете функция repeated(f, n),
;;която връща n-тото прилагане на f.
;; made a mistake in my implt
;; other impl( git)

(define (repeated f n)
  (if (zero? n)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))
;;Пример:

((repeated square 2) 5) ; 625