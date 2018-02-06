#lang scheme
;; task 03
;; ( (0 1 2)
;;;  (3 4 5)....)
(define (for-some? p? l)
  (foldr (lambda(current last) (or (p? current) last)) #f l))

(define (checkMatrix? m k)
  (define (p? x)
    (= 0 (remainder x k)))
  (foldr (lambda(row last) (and (for-some? p? row) last))
         #t m))
(checkMatrix? '((1 2 6) (3 8 9) (10 12 11)) 3) ;; #t
(checkMatrix? '((1 2 4) (3 8 9) (10 12 11)) 3) ; #f