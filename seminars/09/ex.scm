#lang racket
(require rackunit rackunit/text-ui)
;; streams def
(define-syntax delay
    (syntax-rules ()
                  ((delay x) (lambda () x))))

(define (force delayed) (delayed))

(define-syntax cons-stream
  (syntax-rules ()
                ((cons-stream h t) (cons h (delay t)))))

(define head car)
(define (tail s) (force (cdr s)))

(define empty-stream? null?)
(define empty-stream null)


(define (stream->list s)
  (if (empty-stream? s) '()
      (cons (head s) (stream->list (tail s)))))
(define (list->stream l)
  (if (null? l) empty-stream
      (cons-stream (car l) (list->stream (cdr l)))))

;; task 01

(define (stream-range a b)
  (if (> a b) empty-stream
      (cons-stream a (stream-range (+ 1 a) b))))

(define stream-range-tests
  (test-suite
   "Tests for stream-range"

   (check-equal? (stream->list (stream-range 2 1)) '())
   (check-equal? (stream->list (stream-range 0 0)) '(0))
   (check-equal? (stream->list (stream-range 0 3)) '(0 1 2 3))
   (check-equal? (stream->list (stream-range 3 5)) '(3 4 5))
   (check-equal? (stream->list (stream-range 38 42)) '(38 39 40 41 42))))

;(run-tests stream-range-tests)
;; task 02

(define (stream-ref* s n)
  (cond ((null? s) #f)
        ((= n 0) (head s))
        (else (stream-ref* (tail s) (- n 1)))))

#|(define s (list->stream '(1 2 3 4 5)))

(define stream-ref-tests
  (test-suite
   "Tests for stream-ref"

   (check = (stream-ref* s 0) 1)
   (check = (stream-ref* s 1) 2)
   (check = (stream-ref* s 2) 3)
   (check = (stream-ref* s 3) 4)
   (check = (stream-ref* s 4) 5)))

;(run-tests stream-ref-tests)
|#

;; task 03
(define (stream-map* s f)
  (if (empty-stream? s) s
      (cons-stream (f (head s)) (stream-map* (tail s) f))))


#|
(define (identity x) x)
(define (1+ x) (+ x 1))
(define (square x) (* x x))

(define s (list->stream '(1 2 3 4 5)))

(define stream-map-tests
  (test-suite
   "Tests for stream-map"

   (check-equal? (stream->list (stream-map* empty-stream identity)) '())
   (check-equal? (stream->list (stream-map* s identity)) '(1 2 3 4 5))
   (check-equal? (stream->list (stream-map* s 1+)) '(2 3 4 5 6))
   (check-equal? (stream->list (stream-map* s square)) '(1 4 9 16 25))))

(run-tests stream-map-tests)
|#

;; task 04
(define (stream-filter s p?)
  (cond ((empty-stream? s) s)
        ((p? (head s)) (cons-stream (head s) (stream-filter (tail s) p?)))
        (else (stream-filter (tail s) p?))))
#|
(define s (list->stream '(0 1 2 3 4 5)))

  (define stream-filter* stream-filter)

(define stream-filter-tests
  (test-suite
   "Tests for stream-filter"

   (check-equal? (stream->list (stream-filter* empty-stream even?)) '())
   (check-equal? (stream->list (stream-filter* s even?)) '(0 2 4))
   (check-equal? (stream->list (stream-filter* s odd?)) '(1 3 5))))

(run-tests stream-filter-tests)
|#

;; task 05
(define (stream-fold s null-value f)
  (if (empty-stream? s) null-value
      (f (head s) (stream-fold (tail s)
                               null-value
                               f))))
      
;(stream-fold (stream-range 1 10) 0 +)
;(stream-fold (stream-range 1 6) 1 *)

;; task 06
;; done

;; task 07
(define (stream-take s n)
  (if (or (empty-stream? s) (= n 0)) '()
      (cons-stream (head s) (stream-take (tail s) (- n 1)))))

;;(define s (list->stream '(1 2 3 4 5)))
;;(stream->list (stream-take s 3))

;; task 08
(define (stream-drop s n)
  (cond ((empty-stream? s) '())
        ((= n 0) s)
        (else (stream-drop (tail s) (- n 1)))))

;(define s (list->stream '(0 1 2 3 4 5 6 7)))
;(stream->list (stream-drop s 3))


;; task 09
(define zeros (cons-stream 0 zeros))
;(head (tail (tail (tail zeros))))

(define (from x) (cons-stream x (from (+ 1 x))))
(define naturals (from 0))
;(head (tail (tail (tail (tail naturals)))))
(define (map-stream f s)
  (if (empty-stream? s) empty-stream
      (cons-stream (f (head s)) (map-stream f (tail s)))))
(define (1+ x) (+ 1 x))
(define nats (cons-stream 0 (map-stream 1+ nats))) ;; second element is not evaluated ;; no substitution :)
;(head (tail (tail (tail (tail nats)))))

;; task 10
;(stream-ref* nats 42)


;; task 11
(define (zip-streams op s1 s2)
  (cons-stream (op (head s1) (head s2)) (zip-streams op
                                                     (tail s1)
                                                     (tail s2)
                                                     )))
(define fibs (cons-stream 0
                          (cons-stream 1
                                       (zip-streams + fibs
                                                   (tail fibs)))))
#|
(head (tail ; head = 2
       (tail ; head = 1 ( 0 + 1 )
        (tail ; head = 1
         fibs)))) ; head = 0

|#

;;task 12
(define (stream-append s1 s2)
  (cond ((empty-stream? s2) s1)
        ((empty-stream? s1) s2)
        (else (cons-stream (head s1)
                           (stream-append (tail s1)
                                          s2)))))
#|(stream->list (stream-append
               (list->stream '(1 2 3))
               (list->stream '(4 5))))
|#

;; task 13
(define (stream-foldr f null-value s)
  (if (empty-stream? s) null-value
      (f (head s) (stream-foldr f null-value (tail s)))))

(define (stream-join ss) ;; stream of streams
  (stream-foldr stream-append empty-stream ss))
#|
(define s1 (list->stream '(1 2 3)))
(define s2 (list->stream '(3 4 5)))

(define ss (cons-stream s1 (cons-stream s2 empty-stream)))

(stream->list (stream-join ss))
|#

;; task 14
;; (a b c) : a^2 + b^2 = c^2
(define (square x)
  (* x x))
(define (pyth? triple)
  ;;(define (check triple)
    (eq? (square (caddr triple))
     (+ (square (car triple)) (square (cadr triple)))))
 ;(print t)
  #|(or (check t) ;;;;;;;;;;;;;;;;;;;baaaaaaaaaaaaad;;;;;;;;;;;;;;;;;;;;;;;
      (check (list (car t) (caddr t) (cadr t)))
      (check (list (caddr t) (car t) (cadr t)))
      (check (list (caddr t) (cadr t) (car t)))
      (check (list (cadr t) (caddr t) (car t)))
      (check (list (cadr t) (car t) (caddr t)))))
    |#

(define triples
  (cons-stream (list (from 1) (from 1) (from 1) ) ;; nats
               (map-stream
                 (lambda (triple)
                   ;;; here something to change, I need more powerful mixing of 
                   (list (car triple)
                         (tail (cadr triple))
                         (tail (caddr triple))
                 ))
                 triples
                 )))

;(define triple (head (tail (tail (tail (tail triples))))))

(define (get-triple triple)
  (list (head (car triple))
      (head (cadr triple))
      (head (caddr triple))))
;(get-triple triple)

(define pythagorean-triples
  (stream-filter triples (lambda(triple) (pyth? (get-triple triple)))
                 ))
#|  (map-stream (lambda (triple) (list (cons (+ 2 (car (car triple))) (cdr (car triple)))
                                             (cadr triple)
                                             (caddr triple)))
              triples
              ))
|#
;(tail triples)
pythagorean-triples

(get-triple (head (tail (tail (tail (tail pythagorean-triples))))))