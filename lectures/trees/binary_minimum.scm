#lang racket

(define t '(1 (2 () ())
              (3 (4 () ())
                 (5 () ()))))

;;
(define (tree? t)
  (or (null? t)
      (and (list? t) (= 3 (length t))
           (tree? (cadr t))
           (tree? (caddr t)))))

(define (make-tree root r l)
  (list root r l))
(define empty-tree '())
(define empty-tree? null?)
(define left-tree cadr)
(define right-tree caddr)
(define root-tree car)

;; operations
(define (depth-tree t)
  (if (empty-tree? t) 0
      (+ 1 (max (depth-tree (left-tree t))
                (depth-tree (right-tree t))
                   ))))

(define (member-tree x t)
  (cond ((empty-tree? t) #f)
        ((equal? x (root-tree t)) t)
        (else (or (member-tree x (left-tree t))
                  (member-tree x (right-tree t))))))

