#lang scheme
;;ex 07
;; ass lists
;; task 01
#|
(define (run-length-encode l)
  (define (iter current last counter l)
    (cond ((null? l) '())
          ((= last current) (iter (car l) current
                                  (+ 1 counter) (cdr l)))
          (else (cons (cons last counter)
                      (iter (car l) current
                            1 (cdr l))))))
  (iter (cadr l) (car l) 1 l))
|#
(define (run-length-decode v)
   (apply string-append (map (lambda (p) (make-string (car p) (cdr p))) v)))
 
(define (run-length-encode s)
  (let ((n (length s)))
    (let loop ((i (- n 2)) (c (list-ref s (- n 1))) (k 1) (v '()))
      (if (negative? i) (cons (cons c k) v)
          (let ((x (list-ref s i)))
            (if (= c x) (loop (- i 1) c (+ k 1) v)
                (loop (- i 1) x 1 (cons (cons c k) v))))))))

 (run-length-encode '(8 7 7 2 2 2 2 3 3 2)) 