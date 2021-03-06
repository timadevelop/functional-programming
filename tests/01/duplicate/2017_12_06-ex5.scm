#lang scheme
(require rackunit rackunit/text-ui)
;;Variant 1
;; task 01
(define (accumulate combiner null-value term a next b)
  (define (iter a last)
    (if (> a b) last
        (iter (next a) (combiner last a))))
  (iter a null-value))

(define (meetrTwice? f g a b)
  (define (feq? x)
    (= (f x) (g x)))
  (define meetings-count
    (accumulate +
                0
                (lambda(x) (if (feq? x) 1 0))
                a
                (lambda(x) (+ 1 x))
                b))
  (> meetings-count 1)
  )

(define (meetTwice? f g a b)
  ;; iter
  (define (feq? x)
    (= (f x) (g x)))
  
  (define (iter count a)
    (cond ((> count 1) #t)
          ((> a b) #f)
          ((feq? a) (iter (+ 1 count) (+ 1 a)))
          (else (iter count (+ 1 a)))
          ))
  (iter 0 a))
        
    
;;(meetTwice? (lambda(x) x) (lambda(x) (- x)) -3 1) ;f
;;(meetTwice? (lambda(x) x) sqrt 0 5); t

;; task 02
;; ll - list of lists of numbers
;; find repeated in each list numbers
;; and get the greatest of them
(define (map f l)
  (if (null? l)
      l
      (cons (f (car l)) (map f (cdr l)))))

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

;(define (foldl combiner null-value l)
;  (if (null? l) null-value
;     (combiner (car l) (foldl combiner null-value (cdr l)))))


(define (foldl combiner null-value l)
  (if (null? l) null-value
      (foldl combiner (combiner (car l) null-value) (cdr l))))


(define (duplicate? x l)
  (>
   (foldl (lambda (y count) (if (= x y) (+ 1 count) count)) 0 l)
   1))

(define (get-duplicates l)
  (print l)
  (filter (lambda(x) (duplicate? x l)) l))

(define (find-max l)
  (define (iter l last)
    (cond ((null? l) last)
          ((> (car l) last) (iter (cdr l) (car l)))
          (else (iter (cdr l) last))))

  (if (null? l) l
      (iter l (car l))))

(define (maxDuplicate ll)
  (define (iter ll last)
    (cond ((or (null? ll) (null? (car ll))) last)
          ((> (find-max (get-duplicates (car ll))) last)
           (iter (cdr ll) (find-max (get-duplicates (car ll)))))
          (else (iter (cdr ll) last))))
  (iter ll (cadr ll)))
(maxDuplicate '((1 2 3 2) (-4 -4) (5))) ;2 
;(get-duplicates (cadr '((1 9 3 2) (-4 -4) (5))))