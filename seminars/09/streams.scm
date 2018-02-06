#lang scheme

(define the-empty-stream '())
;(define (cons-stream h t) (cons h (delay t)))
(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)

(define s (cons-stream
           1
           (cons-stream
            2
            (cons-stream 3
                         the-empty-stream))))

;(head s) ;; 1
;(tail s) ;; (2.promise)
;(head (tail s)) ;; 2
;(head (tail (tail s))) ;; 3

;; error bcs of substitution model
;; (cons-stream 3 <....all this must be a promise>)
;(define s2 (cons-stream 3 (cons-stream
 ;                          b
  ;                         the-empty-stream)))

;; --> we need a special form for (cons-stream)

(define-syntax delay
  (syntax-rules () ((delay x) (lambda() x))))
(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

;(define s2 (cons-stream
;            3
;            (cons-stream
;             b the-empty-stream))) ;; ? TODO


;; zad
(define (enum a b)
  (if (> a b) the-empty-stream
      (cons-stream a (enum (+ 1 a) b))))

(define (first n s)
  (if (or (empty-stream? s) (= 0 n)) '()
      (cons (head s) (first (- n 1) (tail s)))))
(define (search-stream p? s)
  (cond ((empty-stream? s) #f)
        ((p? (head s)) s)
        (else (search-stream p? (tail s)))))


;; indirect
(define (generate-fibs a b)
  (cons-stream a (generate-fibs b
                                (+ a b))))
(define fibs (generate-fibs 0 1))
;; high-order procedures
(define (map-stream f s)
  (cons-stream (f (head s)) (map-stream f (tail s))))

(define (filter-stream p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-stream
                             p?
                             (tail s)))
      (filter-stream p? (tail s))))

(define (zip-stream op s1 s2)
  (cons-stream (op (head s1) (head s2))
               (zip-stream (tail s1) (tail s2))))

;; direct resursion
(define (1+ x) (+ x 1))
(define ones (cons-stream 1 ones))
(define nats (cons-stream 0
                          (map-stream 1+ nats)))
