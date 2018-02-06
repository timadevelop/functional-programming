#lang scheme
;; task 2
;; ( (1 2 3) (3 4) (5 3) )
;; (( 1 2 3 2) (-4 -4) (5)) ->
;; -> ((2 2) (-4 -4) #f) ->
;; -> (2 -4) -> 2
(define (duplicate? x l)
  (> (foldr (lambda(e last)
              (if (= e x) (+ 1 last) last)) 0 l)
     1))
(define (filter p? l)
  (cond ((null? l) '())
        ((p? (car l)) (cons (car l) (filter p?
                                            (cdr l))))
        (else (filter p? (cdr l)))))

;(duplicate? 1 '( 2 3 1 3 1))
(define (get-duplicates l)
  (filter (lambda(x) (duplicate? x l)) l))
;(get-duplicates '(1 2 3 4 5 2 3))
(define (get-max l)
  (define (iter l last)
    (cond ((null? l) last)
          ((> (car l) last) (iter (cdr l) (car l)))
          (else (iter (cdr l) last))))
  (if (null? l) #f
      (iter l (car l))))

;(get-max (get-duplicates '(1 2 3 4 5 2 3)))

(define (getDuplicates ll)
  (filter (lambda(x) (not (eq? #f x)))
          (foldr (lambda (l last) (cons (get-max (get-duplicates l)) last)) '() ll)))
(define (maxDuplicate ll)
  (get-max (getDuplicates ll)))

;(maxDuplicate '( (1 2 1) (2 3 3) (-4 -4) (5)))
(maxDuplicate '((1 2 3 2) (-4 -4) (5))) ;; 2
(maxDuplicate '((1 2 3) (-4 -5 -6) ())) ;; #f