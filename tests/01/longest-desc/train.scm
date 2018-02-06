#lang scheme
(require rackunit rackunit/text-ui)


; Задача.
; Да се напише функция (longest-descending l), която намира низходящо сортиран
; подсписък на списъка от числа l с максимална дължина. Ако съществуват няколко
; такива подсписъка, функцията да върне първия отляво надясно.

(define (maximum l)
  (define (iter l last)
    (cond ((null? l) last)
          ((> (car l) last) (iter (cdr l) (car l)))
          (else (iter (cdr l) last))))
  (iter l (car l)))
;(maximum (list 34 35 321 4 4 132 57 3234 1221 42364 31))
(define (append l1 l2)
  (cond ((null? l2) l1)
        ((null? l1) l2)
        (else (cons (car l1) (append (cdr l1) l2)))))
;(append (list 1 2 3) (list 3 2 1))
(define (pop-first p? l)
  (cond ((null? l) '())
        ((p? (car l)) (pop-first (lambda(x) #f) (cdr l)))
        (else (cons (car l) (pop-first p? (cdr l))))))
      

;(pop-first (lambda(x) (= 2 x)) (list 1 2 3 4 2))

(define (pop-first-element x l)
  (pop-first (lambda (e) (= e x)) l))
(define (selection-sort l)
  (if (null? l) '()
      (cons (maximum l) (selection-sort (pop-first-element (maximum l) l)))))

(selection-sort (list 1 4 5 23 1 23 8))