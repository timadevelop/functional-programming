#lang scheme
;;
;; problem 2.
;;
;; Напишете функция (cross-out m), която по дадена матрица от числа m с размери M*N
;; генерира всички M*N на брой матрици, които могат да се получат от m със задраскване
;; на някой ред и някой стълб. Редът на върнатите матрици няма значение.
;; Пример:
;; (cross-out '((1 2 3 4)      '(((6 7 8)  ((5 7 8)  ((5 6 8)  ((5 6 7)  ((2 3 4)
;;              (5 6 7 8)   ->    (0 1 2))  (9 1 2))  (9 0 2))  (9 0 1))  (0 1 2)) ...
;;              (9 0 1 2)))       ; и още 7 малки матрици
;; r c
;; 0 0
;; 0 1
;; 0 2
;; 0 ..
;; 1 0
;; 1 1
;; 1 2
;; 1 ..
;; 2 0
;; 2 1
;; 2 2
;; 2 ..
;; .. 0
;; .. 1
;; .. ..

(define (remove-row l i)
  (cond ((null? l) '())
        ((= 0 i) (remove-row (cdr l) -1))
        (else (cons (car l) (remove-row (cdr l) (- i 1))))))

(define (remove-col m i)
  (if (null? m) '()
      (cons (remove-row (car m) i) ;; without first element of current row
            (remove-col (cdr m) i))))

(define (count-cols m) (length (car m)))

(define (cross-out m)
  (define (iter-col m ci)
    (if (< ci 0) '()
        (cons (remove-col m ci) (iter-col m (- ci 1)))))
  (define (iter-row ri columns)
    (if (< ri 0) '();(iter-col (remove-row m ri) columns)
        (append (iter-col (remove-row m ri) columns) (iter-row (- ri 1) columns))))
  
  (iter-row (- (length m) 1) (- (count-cols m) 1)))

(cross-out '((1 2 3 4)
             (5 6 7 8)
             (9 0 1 2)))