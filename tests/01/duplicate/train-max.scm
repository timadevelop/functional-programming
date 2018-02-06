#lang scheme
(require rackunit rackunit/text-ui)

; Задача.
; Да се напише функция (max-duplicate ll), която по списък от списъци от
; цели числа ll намира най-голямото от тези числа, които се повтарят в
; рамките на списъка, в който се срещат. Ако в нито един списък няма
; повтарящи се числа, функцията да връща #f.

;; ((1 2 3 2) (-4 -4) (5))) -> find-dup-> ((2) (-4) (#f))
;; find-max -> 2
(define (foldr f null-value l)
  (if (null? l) null-value
      (f (car l) (foldr f null-value (cdr l)))))

(define (duplicate? x l)
  (>
   (foldr (lambda(curr last) (if (= curr x) (+ 1 last) last)) 0 l)
   1))

;(duplicate? 4 '(1  4 5 2 6 2))

(define (filter p? l)
  (cond ((null? l) '())
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (duplicates l)
  (filter (lambda(x) (duplicate? x l)) l))
;(duplicates '(1 2 34 34 2 6 78 34 1))
(define (maximum l)
  (define (iter l last)
    (cond ((null? l) last)
          ((> (car l) last) (iter (cdr l) (car l)))
          (else (iter (cdr l) last))))
  (if (null? l) #f
      (iter l (car l))))

;(maximum (duplicates '(1 2 34 34 2 6 78 34 1)))


(define (max-duplicate ll)
 ; (for each l:
  ;  (append-to-list (find-max ( find-duplicates l))))
  (maximum (filter number?
                   (map (lambda(l) (maximum(duplicates l)))
                        ll
             ))))
  
(define max-duplicate-tests
  (test-suite
   "Tests for max-duplicate"

   (check = (max-duplicate '((1 2 3 2) (-4 -4) (5))) 2)
   (check = (max-duplicate '((1 2 3) (-4 -4) (5 2) (5 6))) -4)

   (check-false (max-duplicate '((1 2 3) (-4 -5 -6) ())))
   (check-false (max-duplicate '((1 2 3) (-4 -5 -6) (2))))))

(run-tests max-duplicate-tests)