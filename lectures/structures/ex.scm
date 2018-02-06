;; 2.25
(define l (list 1 3 (list 5 7) 9)) 
(cadr (car (cdr (cdr l))))
; 7
;
(define l (list (list 7)))
(car (car l))
;;
;;
(define l (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;(print (cadr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))

;; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
;; append -> (1 2 3 4 5 6)
;; cons x y -> ((1 2 3) 4 5 6) ! Intresting !
;; list x y -> ((1 2 3) (4 5 6))

;; 2.27
(define (foldr op nv l)
  (if (null? l) nv
    (op (car l) (foldr op nv (cdr l))))
)

(define (foldl op nv l)
  (if (null? l) nv
 	     (foldl op (op (car l) nv) (cdr l)))
)

(print (foldl * 1 (list 1 2 3)))

(define (reversee l)
  (foldl cons null l)
)

(print (reversee (list 1 2 3 54)))
