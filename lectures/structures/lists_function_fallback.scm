(list (list 0 2 3) (list 4 5 6))

(define (list-reff l n)
  (if (< (- (length l) 1) n)
    (error "err")

  (if (= n 0)
    (car l)
    (list-reff (cdr l) (- n 1))
  )
  )
)

(define (len l)
  (if (null? l)
    0
    (+ 1 (len (cdr l)))))

(define (appendd l r)
  (if (null? l)
    r
    (cons (car l) (appendd (cdr l) r))
  )
)



(print (appendd (list 1 2 3 4 5) (list 6 7 8)))
;(print (len (list 1 2 3 4 5)))
;(print (list-reff (list 1 2 3 4 5) 5))
