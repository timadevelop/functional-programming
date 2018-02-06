#lang scheme
(require rackunit rackunit/text-ui)
(define (accumulate combiner null-value term a next b)
  (define (iter a last)
    (if (> a b)
        last
        (iter (next a) (combiner last (term a)))))
  (iter a null-value))

(define (sum a b)
  (accumulate + 0 (lambda(x) x) a (lambda(x) (+ 1 x)) b))


;; 8
;;Напишете предикат for-all?(predicate, a, b),
;;който проверява дали за всяко цяло число
;;в интервала [a, b] предикатът predicate е истина.
;;
(define (for-all? predicate a b)
  (cond ((> a b) #t)
        ((not (predicate a)) #f)
        (else (for-all? predicate (+ a 1) b))))
;; other solution (from repo)
(define (for-all-repo? predicate a b)
  (or (> a b)
      (and (predicate a) ;; here and is not working with substitution model,
           ;;so if (pr a) will be #f next iterative call will not be evaluated.
           (for-all? predicate (+ a 1) b))))
;; other solution
(define (for-all-accum? predicate a b)
  (accumulate (lambda (x y) (and x y))
              #t
              predicate
              a
              (lambda(x) (+ 1 x))
              b))


(define for-all?-tests
  (test-suite
    "Tests for for-all?"

    (check-true (for-all? (lambda (x) (> x 0)) 2 98))
    (check-true (for-all? (lambda (x) (< x 0)) -10 -1))
    (check-true (for-all? (lambda (x) (= 0 (* x 0))) -3 15))
    (check-true (for-all? (lambda (x) (= 0 (* x 1))) 2 1))

    (check-false (for-all? (lambda (x) (= x 3)) 1 5))
    (check-false (for-all? (lambda (x) (= x 13)) 1 5))
    (check-false (for-all? (lambda (x) (< x 3)) -5 42))))

(run-tests for-all?-tests)