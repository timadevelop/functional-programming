#lang scheme

;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;


(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (count-digits n)
  (define (helper last result)
    (if (= last 0)
    	result
    	(helper (quotient last 10) (+ result 1))
    )
  )
  (helper n 0))

(define (middle-digit n)
  (define (iter i max last)
    (if (= i max) (remainder last 10)
        (iter (1+ i) max (quotient last 10)))
  )
  (if (= 0 (remainder (count-digits n) 2))
      -1
      (iter 0 (1- (round (/ (count-digits 452) 2))) n)))
;; test
;(middle-digit 452)
;(middle-digit 4712)

;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;

(define (foldr f null-value l)
  (if (null? l) null-value
      (f (car l) (foldr f null-value (cdr l)))))

(define (for-every? p? m)
  (foldr (lambda(x last) (and (p? x) last)) #t m)) 

(define (for-some? p? l)
  (foldr (lambda(x last) (or (p? x) last)) #f l))

(define (get-columns m) (length (car m)))

(define (get-row i m) (list-ref m i))
(define (get-column i m) (map (lambda (row) (list-ref row i)) m))

(define (transpose m)
  (accumulate cons '() 0 (- (get-columns m) 1)
              (lambda (i) (get-column i m)) 1+))

;(transpose '((1 2 3 4) (5 6 7 8)))
(define (sum l)
  (foldr + 0 l))
(define (remove l x)
  (cond ((null? l) '())
        ((equal? x (car l)) (remove (cdr l) '()))
        (else (cons (car l) (remove (cdr l) x)))))

(define (match? l)
  (for-some? (lambda(el) (= el (sum (remove l el)))) l))

(define (count-cols m)
  (foldr (lambda(col last)
           (if (match? col) (1+ last)
               last))
         0 (transpose m)))

;; test
;(count-cols '((1 2 3 6) (2 3 4 2) (3 4 5 4))) ;; 2

;;;;;;;;;;;
;;Problem 3
;;;;;;;;;;;
;; '(1 2 3 f)

(define (is-em? l op f)
  (and (for-every? (lambda(el) (memq (f el) l)) l)
       (for-every? (lambda(x) (for-some? (lambda(el) (eq? (op (f el) (f x)) (f (op el x)))) (remove l x)))
                  l)))

;(is-em? '(0 1 4 6) + (lambda(x) (remainder x 3)))

;;;;;;;;;;;
;;Problem 4
;;;;;;;;;;;
; '(1 (2 () ()
;     (3 (4 () ())
;        (5 () ())))
;; tree definitions
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define empty-tree null)
(define empty-tree? null?)
(define make-tree list)
(define left-tree cadr)
(define right-tree caddr)
(define root-tree car)

(define (families-alike? t)
  (define (root-left t) (root-tree (left-tree t)))
  (define (root-right t) (root-tree (right-tree t)))
  (define (find p? t)
    (cond ((empty-tree? t) #f)
          ((p? t) t)
          (else (or (find p? (left-tree t))
                    (find p? (right-tree t))))))

  (define (p? left right)
    (and (tree? left)
         (tree? right)
         (eqv? (root-tree left) (root-tree right))
         (eqv? (root-left left) (root-left right))
         (eqv? (root-right left) (root-right right))))

  (define (iter l r)
    (if (find (lambda(tree) (p? l tree)) r)
        (find (lambda(tree) (p? l tree)) r)
        (or (iter (left-tree l) r)
            (iter (right-tree l) r))))
        
  
  
  (not (null? (iter (left-tree t) (right-tree t))))
)

(define t '(1 (2 (3 () ())
                 (4 (6 () ())
                    ()))
              (5 (2 (2 () 6)
                    (4 () ()))
                 (7 () ()))))

;(families-alike? t)