#lang racket
;; ex 2.3////
(define (count-leaves x)
  (cond ((null? x) 0)
        ((list? x) (+ (count-leaves (car x))
                      (count-leaves (cdr x))))
        (else 1)))
 


(define (scale-tree t x)
  (cond ((null? t) '())
        ((list? t) (cons (scale-tree (car t) x)
                         (scale-tree (cdr t) x)))
        (else (* x t))))
(define (scale-tree-map t x)
  (map (lambda(sub) (if (list? sub)
                        (scale-tree-map sub x)
                        (* x sub)
                        ))
       t))

(define (ssquare-tree t)
  (map (lambda (sub) (if (list? sub)
                         (square-tree sub)
                         (* sub sub)))
       t))

(define (tree-map f t)
  (cond ((null? t) '())
        ((list? t) (cons (tree-map f (car t))
                         (tree-map f (cdr t))))
        (else (f t))))
(define (square x)
  (* x x))
(define (square-tree tree)
  (tree-map square tree))

(define (accumulate combiner null-value l)
  (if (null? l) null-value
      (combiner (car l)
                (accumulate combiner null-value (cdr l)))))

;(accumulate + 0 (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high) '()
      (cons low (enumerate-interval (+ 1 low) high))))
;(enumerate-interval 2 7)

(define (enumerate-tree t)
  (cond ((null? t) t)
        ((list? t) (append (enumerate-tree (car t))
                         (enumerate-tree (cdr t))))
        (else (list t))))

;(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))
(define (sum-odd-squares t)
  (accumulate + 0
              (map square (filter odd?
                                   (enumerate-tree t)))))


(define (map p sequence)
  (accumulate (lambda(x y) (cons (p x) (map p (cdr sequence))))
              '() sequence))

;(map square '(1 2 3))

(define (appendd l1 l2)
  (accumulate cons l2 l1))
;(appendd (list 1 2 3) (list 3 4 5))
;(define (length seq)
;  (accumulate (lambda(x y) (+ 1 y)) 0 seq))
;(length (list 1 2 3 4))

(define (ccount-leaves t)
  (define (leave? t) (not (list? t)))
  (accumulate (lambda(x y) (+ 1 y)) 0
              (enumerate-tree t)))

;(ccount-leaves (list (list 1 2 3) 4 (list 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;TREEES;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (entry left right)
;(define entry car)
;(define left-branch cadr)
;(define right-branch caddr)
;(define (make-tree entry l r)
;'  (list entry l r))




;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;Lecture;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
#|
(define (make-rat n d)
  (let* ((g (gcd n d))
         (numer (quotient n g))
         (denom (quotient d g)))
  (lambda(prop . params)
    (case prop
      ('get-numer numer)
      ('get-denom denom)
      ('print (cons numer denom))
      ('* (let ((r (car params)))
            (make-rat (* numer (r 'get-numer))
                      (* denom (r 'get-denom)))))
      ))))
|#
;; or

(define (make-rat n d)
  (let* ((g (gcd n d))
         (numer (quotient n g))
         (denom (quotient d g)))
  (define (self prop . params) ;; open recursive
    (case prop
      ('get-numer numer)
      ('get-denom denom)
      ('print (cons numer denom))
      ('* (let ((r (car params)))
            (make-rat (* (self 'get-numer) (r 'get-numer))
                      (* (self 'get-denom) (r 'get-denom)))))
      ))
    self ;; return
    ))

;;(define r (make-rat 10 5))
;;(define r2 (make-rat 5 10))
;;(r 'get-numer)
;;(r 'get-denom)
;;(r 'print)
;;((r '* r2) 'print)

;; binary trees
;;  /1\
;; 2  /3\
;;   4   5
(define tree '( 1 (2 '() ())
     (3 (4 '() '())
        (5 '() '()))))

(define (tree? t)
  (or (null? t)
      (and (list? t) (= (length t) 3))
      (tree? (cadr t)) ;; or because of leaves
      (tree? (caddr t))))

;(tree? tree)
(define empty-tree '())
(define (make-tree root l r)
  (list root l r))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (depth-tree t)
  (if (empty-tree? t) 0
      (+ 1 (max (depth-tree (left-tree t))
                (depth-tree (right-tree t))))))

(define (memq-tree x t) ;; eq
  (cond ((empty-tree? t) #f)
        ((eq? (root-tree t) x) t)
        (else (or (memq-tree x (left-tree t))
                  (memq-tree x (right-tree t))))))

(define (path-tree x t)
  (define (cons#f h t) (and t (cons h t)))
  (cond ((empty-tree? t) #f)
        ((eq? x (root-tree t)) (list x))
        (else (cons#f (root-tree t)
                      (or (path-tree x (left-tree t))
                          (path-tree x (right-tree t)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;associative lists;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;'((1 . 2) (2 . 3) (3 . 4))

(define (make-alist f keys) ;; alist from function
  (map (lambda(x) (cons x (f x))) keys))

(define sqrs (make-alist square '(1 2 3 4 5)))

(define (keys alist)
  (map car alist))
(define (values alist)
  (map cdr alist))
;(keys sqrs)
;(values sqrs)

(define (ass p? key alist) ;; find by key
  (cond ((null? alist) #f)
        ((p? key (car (car alist))) (car alist))
        (else (assoc key (cdr alist)))))
(define (assoc key alist)
  (ass equal? key alist))
(define (assv key alist)
  (ass eqv? key alist))
(define (assq key alist)
  (ass eq? key alist))

(define (del-first p? key alist)
  (cond ((null? alist) '())
        ((p? key (car alist)) (del-first (lambda(x) #f) key (cdr alist)))
        (else (cons (car alist) (del-first p? key alist)))))

(define (del-assoc key alist)
  (filter (lambda(pair) (not (equal? key
                                     (car pair))))
          alist))

(define (add-assoc key value alist)
  (cons (cons key value) (del-assoc key alist)))

(define (search p? l)
  (and (not (null? l))
       (or (p? (car l)) ;; car l is a pair
           (search p? (cdr l)))))

(define (all p? l)
  (not (search (lambda (x) (not (p? x))) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; GRAPHS ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (vertices g) (map car g))

(define (children v g)
  (cdr (assq v g)))
(define (edge? u v g)
  (memq v (children u g)))
(define (map-children v f g)
  (map f (children v g)))
(define (search-child v f g)
  (search f (children v g)))

(define (parents v g)
  (filter (lambda(u) (edge? ui v g)) (vertices g)))
