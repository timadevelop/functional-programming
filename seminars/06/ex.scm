#lang scheme


;; task 01
(define (dimensions m)
  (define (list-ref i l)
    (if (= i 0) (car l)
        (list-ref (- i 1) l)))
  (define rows-count length)
  (define (get-row i m)
    (list-ref i m))
  (define (col-count m)
    (length (get-row 0 m)))
  (cons (rows-count m) (col-count m))
)

;(dimensions '((1 2 3) (4 5 6))) ;; (2.3)

;; task 02

(define (reverse-columns m)
  (define (foldl f null-value l)
    (if (null? l) null-value
        (foldl f (f (car l) null-value) (cdr l))))
  (define (reverse l)
    (foldl cons '() l))
  (map reverse m))
;(reverse-columns '((1 2 3) (4 5 6) (7 8 9)))

;; task 03
(define (nth-column m n)
  (define (list-ref i l)
    (if (= i 1) (car l) (list-ref (- i 1) (cdr l))))
  (foldr (lambda (row last) (cons (list-ref n row) last)) '() m))

;(nth-column '((1 2 3) (4 5 6) (7 8 9)) 2)

;; task 04

(define (main-diagonal m)
  (define (list-ref i l)
    (cond ((null? l) l)
          ((= i 0) (car l))
          (else (list-ref (- i 1) (cdr l)))))
  (define (rec i l)
    (if (null? l) l
        (cons (list-ref i (car l)) (rec (+ 1 i) (cdr l)))))
  
  (rec 0 m))
;(main-diagonal '((1 2 3) (4 5 6) (7 8 9))) ;; 1 5 9

;; task 05

(define (transpose m)
  (define (list-ref i l)
    (cond ((null? l) l)
          ((= 0 i) (car l))
          (else (list-ref (- i 1) (cdr l)))))
  
  (define (nth-column m n)
    (foldr (lambda(row last) (cons (list-ref n row) last))
           '() m))
  (define (iter i)
    (if (> i (length m)) '()
        (cons (nth-column m i) (iter (+ i 1)))))
  (iter 0)
)

(transpose '((1 2 3) (4 5 6))) ; '((1 4) (2 5) (3 6))

