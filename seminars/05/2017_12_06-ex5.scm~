#lang scheme
(require rackunit rackunit/text-ui)
;; RULE, vlad, rule: read task, write solution, see other solutions :)
;; ex 4

;; task 01 - tested
(define (length-rec l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define (length l)
  (define (iter list last)
    (if (null? list)
        last
        (iter (cdr list) (+ 1 last))))
  (iter l 0))

;; task 02 
;; recurvice
(define (sum-elements-rec l)
  (if (null? l)
      0
      (+ (car l) (sum-elements-rec (cdr l)))))
;; iterative
(define (sum-elements-iter l)
  (define (iter l last)
    (if (null? l)
        last
        (iter (cdr l) (+ last
                         (car l)))))
  (iter l 0))

;; task 03
;; Да се дефинира функция member?(l, x),
;; която проверява дали x е елемент на списъка l.
(define (member-r? l x)
  (if (null? l)
      #f
      (or (= x (car l)) (member-r? (cdr l) x))))
;; alternative
(define (member? l x)
  (and (not (null? l))
       (or (= (car l) x)
           (member? (cdr l) x))))

;; task 04
(define (last l)
  (cond ((null? l) '())
        ((null? (cdr l)) (car l))
        (else (last (cdr l)))))

;; task 05
(define (nth l n)
  (cond ((null? l) '()) ;; null-test :)
        ((= n 0) (car l))
        (else (nth (cdr l) (- n 1)))))
;; from repo
(define (ntth l n)
  (if (zero? n)
      (car l)
      (nth (cdr l) (- n 1))))
;;(nth (list 1 2) 3)

;; task 06
(define (scale l x)
  (if (null? l)
      '()
      (cons (* x (car l)) (scale (cdr l) x))))

;; task 07
(define (reverse l)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (cons (car l) result))))
  (iter l '()))

;; task 08
(define (add-last l x)
  (if (null? l)
      (cons x '())
      (cons (car l) (add-last (cdr l) x)))
  )

;; task 09
(define (adppend l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))
;; better, I think
(define (append l1 l2)
  (if (null? l2) l1
      (if (null? l1) l2
          (cons (car l1) (append (cdr l1) l2)))))

;; task 10
(define (map l f)
  (if (null? l)
      '()
      (cons (f (car l)) (map (cdr l) f))))
;(map (list 1 2 3) (lambda(x) (* x x)))

;; task 11
(define (filter l p)
  (cond ((null? l) '())
        ((p (car l)) (cons (car l) (filter (cdr l) p)))
        (else (filter (cdr l) p))))

;;(filter (list 1 2 34 54 67) (lambda(x) (< x 10)))

;; task 12
(define (accumulate combiner null-value term l)
  (if (null? l)
      null-value
      (combiner (term (car l)) (accumulate combiner
                                           null-value
                                           term
                                           (cdr l)))))

(accumulate + 0 (lambda (x) x) (list 1 2 3))