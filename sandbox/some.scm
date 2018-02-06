(define (sum a b term next)
  (if (> a b)
    0
    (+ (term a) (sum (next a) b term next)))) 

(define (product a b term next)
  (if (> a b)
    1
    (* (term a) (product (next a) b term next))))







(define (accumulate op nv term a next b)
  (if (> a b)
    nv
    (op (term a) (accumulate op nv term (next a) next b))))
