#lang scheme
;;
;; problem 1.
;;
;(( 1 5 2)     ((1  5 2)
; ( 2 3 8)  ->  (0 -7 4)   ; получено от (ред1 * (-2)) + ред2
; (-2 0 4))     (0 10 8)) 

;; (r1*k) + r2 = 0
;; k = -r2 / r1
;;

;; find-first row with 1st el != 0
;; found r1 ->
;; for-each row except found one.
;; k = -r2 / r1
;; r2 = (r2 + (r1 * k))
;; k = -r3 / r1
;; r3 = (r3 + (r1 * k))

(define (remove l p?)
  (cond ((null? l) '())
        ((p? (car l)) (remove (cdr l) (lambda(x) #f)))
        (else (cons (car l) (remove (cdr l) p?)))))

(define (find l p?)
  (cond ((null? l) '())
        ((p? (car l)) (car l))
        (else (remove (cdr l) p?))))

(define (row-reduce m)
  (define (p? row) (not (= 0 (car row))))
  (define (get-k r base) (/ (- (car r)) (car base)))
  (define (mult l c) (map (lambda(x) (* c x)) l))
  (define (iter base other)
    (map (lambda(row)
           (map + row (mult base (get-k row base))))
         other))

  (cons (find m p?) (iter (find m p?) (remove m p?)))
)

(define m '(( 1 5 2 ) ( 2 3 8 ) (-2 0 4 )))

(row-reduce m)