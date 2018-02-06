#lang scheme
(require rackunit rackunit/text-ui)
;; task 01
(define (maximum l)
  (define (iter l last)
    (cond ((null? l) last)
          ((> (car l) last) (iter (cdr l) (car l)))
          (else (iter (cdr l) last))))
  (if (null? l) '()
      (iter l (car l)))
)

;(maximum (list 32 64 43 36))

;; tests
(define maximum-tests
  (test-suite
    "Tests for maximum"

    (check = (maximum '(2)) 2)
    (check = (maximum '(5 3 5 5)) 5)
    (check = (maximum '(8 4 92 82 8 13)) 92)
    (check = (maximum '(8 4 82 12 31 133)) 133)))

;(run-tests maximum-tests)

;; task 02
(define (remSove l x) ;; this is my first attempt... error
  (define (append l1 l2)
    (cond ((null? l2) l1)
          ((null? l1) l2)
          (else (cons (car l) (append (cdr l) l2)))))

  (define (push-back l x)
    (if (null? l)
        (cons x '()) ;; ! x is not null :D
        (cons (car l) (push-back (cdr l) x))))
  
  (define (iter l result)
    (cond ((null? l) result)
          ((equal? x (car l)) (append result (cdr l)))
          (else (iter (cdr l) (push-back result (car l))))
          ))
  (iter l '()))
;;
;; filter -> remove
(define (remove l x)
  (cond ((null? l) '())
        ((equal? x (car l)) (remove (cdr l) '()))
        (else (cons (car l) (remove (cdr l) x)))))

;; task 03
#|

;;;;
;;;; DANGEROUS !!!
;;;; very stupid code in this section :\
;;;;

(define (nth l n)
  (cond ((null? l) '())
        ((= n 0) (car l))
        (else (nth (cdr l) (- n 1)))))

(define (find-min-value-index l)
  (define (iter index min-index last-min list)
    (cond ((null? list) min-index)
          ((< (car list) last-min) (iter (+ 1 index) index (car list) (cdr list)))
          (else (iter (+ 1 index) min-index last-min (cdr list)))))
  (if (null? l) -1 (iter 0 0 (car l) l))
)

;(find-min-value-index (list 2 3 4 0 6 1 5 1 10))
(define (cdr-from i l)
  (if (= 1 i)
      (if (null? l) l (cdr l))
      (cdr-from (- i 1) (cdr l))
      ))

;(cdr-from 2 (list 1 2 3 4 5))

(define (swap i1 i2 list)
  (let ((v1 (nth list i1))
        (v2 (nth list i2))
        )
    (define (iter index l)
      (cond ((null? l) '())
            ((= index i1) (cons v2 (iter (+ 1 index) (cdr l))))
            ((= index i2) (cons v1 (iter (+ 1 index) (cdr l))))
            (else (cons (car l) (iter (+ 1 index) (cdr l))))
    ))
    (iter 0 list)
    ))

;(swap 2 11 (list 1 2 3 4 5 6 7 8 9 10 11 12))
(find-min-value-index
 (cdr-from 2 (list 23 34 5 9 0 1)))
(define (selection-sort l comp)
  (define (iter index list)
    (let
      ((min-index (+ 1 (find-min-value-index (cdr-from (+ 1 index) list))))) ;; search in swapped list
      (cond ((or (= min-index -1) (> index (length list))) list)
            ((< (nth list index) (nth list min-index)) ;; if we found smthing <
             (iter (+ 1 index) (swap min-index index list)))
            (else (iter (+ 1 index) list))
            )
    ))
  (iter 0 l))

;;;;
;;;; The end of dangerous section
;;;;
|#

;;; I was so stupid....

(define (get comp l)
  (define (iter l last)
    (cond ((null? l) last)
          ((comp (car l) last) (iter (cdr l) (car l)))
          (else (iter (cdr l) last))))
  (iter l (car l)))

;(get > (list 345 464 234 7 23 46 234 7 -1 23 2532 123))
     
(define (pop-first predicate? l)
  (cond ((null? l) '())
        ((predicate? (car l)) (pop-first (lambda(x) #f) (cdr l)))
        (else (cons (car l) (pop-first predicate? (cdr l)))))
)
;(pop-first (lambda (x) (= x 20)) (list 1 3 4 2 23 20 324 3423 24 20 1))
(define (pop-value v l)
  (pop-first (lambda (x) (= v x)) l))


(define (selection-sort l comp)
  (cond ((null? l) '())
        (else (cons (get comp l) (selection-sort (pop-value (get comp l) l)
                                                 comp)))))
      
;(selection-sort (list 23 34 5 9 0 1) >)

;; tests
(define selection-sort-tests
  (test-suite
    "Tests for selection-sort"

    (check-equal? (selection-sort '(42) <) '(42))
    (check-equal? (selection-sort '(6 6 6) <) '(6 6 6))
    (check-equal? (selection-sort '(1 2 3 4 5 6) <) '(1 2 3 4 5 6))
    (check-equal? (selection-sort '(6 5 4 3 2 1) <) '(1 2 3 4 5 6))
    (check-equal? (selection-sort '(3 1 4 6 2 5) <) '(1 2 3 4 5 6))
    (check-equal? (selection-sort '(5 2 5 1 5 2 3) <) '(1 2 2 3 5 5 5))))

;(run-tests selection-sort-tests)

;; task 03 end

;; task 04
;; Да се дефинира функция partition(p, l), която връща списък от два подсписъка, където:
;; първият съдържа всички елементи на l, които удовлетворяват предиката p.
;; вторият съдържа всички останали елементи на l.
;; Например, (partition even? '(1 2 3 4 5 6 7)) връща '((2 4 6) (1 3 5 7)).

(define (partition p? l)
  (define (filter l p?)
    (cond ((null? l) '())
          ((p? (car l)) (cons (car l) (filter (cdr l) p?)))
          (else (filter (cdr l) p?))))
  (list (filter l p?) (filter l (lambda(x) (not (p? x))))))
;(partition even? '(1 2 3 4 5 6 7))

(define partition-tests
  (test-suite
    "Tests for partition"

    (check-equal? (partition even? '(1 2 3 4 5 6 7)) '((2 4 6) (1 3 5 7)))
    (check-equal? (partition odd? '(1 3 3 7 42)) '((1 3 3 7) (42)))
    (check-equal? (partition odd? '(3)) '((3) ()))
    (check-equal? (partition even? '()) '(() ()))
    (check-equal? (partition (lambda (x) (< x 4)) '(1 2 3 4 5 6 7))
                  '((1 2 3) (4 5 6 7)))))

;(run-tests partition-tests)
;; task 05
;;Да се дефинира функция flatten(l), която приема списък от атоми (числа) и списъци с атоми l и връща списък с всички атоми.;;
;;Например, (flatten '((1 2) 3 (4 5) (6 7))) връща '(1 2 3 4 5 6 7).
(define (atom? x)
  (and (not (list? x))))

(define (flatten l)
  (define (append l1 l2)
    (cond ((null? l2) l1)
          ((null? l1) l2)
          (else (cons (car l1) (append (cdr l1) l2)))))
  (cond ((null? l) '())
        ((atom? l) (cons l '()))
        (else (append (flatten (car l)) ;; null of (car l) remove
                      (flatten (cdr l))))))

;;(flatten '((1 2) 3 (4 5) (6 7)))
;; task 06
(define (map-deep f l)
  (cond ((null? l) '())
        ((atom? l) (f l))
        (else (cons (map-deep f (car l)) ;; saves null of (car l)
                    (map-deep f (cdr l))))))
;;(map-deep (lambda(x) (* x x)) '((1 2 (3 4)) 5))

;; task 07
(define (zip a b)
  (cond ((or (null? b) (null? a)) '())
        (else (cons (list (car a) (car b))
                    (zip (cdr a) (cdr b))))))

;(zip '(1 3 5) '(2 4 6 8))

;; task 08

(define (filter l p?)
  (cond ((null? l) '())
        ((p? (car l)) (cons (car l) (filter (cdr l) p?)))
        (else (filter (cdr l) p?))))

  (define (ne? a)
    (lambda(x) (not (= x a))))

(define (foldr f null-value l)
  (if (null? l) null-value
      (f (car l) (foldr f null-value (cdr l)))))

(define (duplicate? x l)
  (>
   (foldr (lambda(e last) (if (= x e) (+ 1 last) last)) 0 l)
   1))
;(duplicate? 5 '( 2 3 5 23 3 2))

(define (remove-duplicates l)
  ; (for-each x: (duplicate? x l)
  (filter l (lambda(x) (not (duplicate? x l)))))

;(remove-duplicates (list 1 2 3 4 5 6 7 8 1 2 3 4))

;; task 09
;;Да се дефинира функция chunk(l, n),
;;която разбива списъка l на подсписъци с дължина n.
;Последният подсписък може да е с дължина по-малка от n,
;;ако дължината на l не е кратна на n.
;Например, (chunk '(1 1 1 2 2 2 3 4) 3) връща '((1 1 1) (2 2 2) (3 4)).
; '(1 1 1 2 2 2 3 4) -> ( (1 1 1) 

#|(define (chunk l n)

  (define (rec l i)
    (cond ((null? l) '())
          ((= i n) (cons (cons (car l) (rec (cdr l) (- i 1))) null))
          ((= i 0) (
        
  (chunk '(1 1 1 2 2 2 3 4) 3)|#

