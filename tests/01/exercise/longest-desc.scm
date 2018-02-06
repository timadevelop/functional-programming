#lang scheme



(define (longestDescending l)
  (define (iter l candidate last-longest)
    (cond ((null? l) last-longest)
          ((or (null? candidate) (< (car l) (last candidate)))
           (let ((next-candidate (append candidate (list (car l)))))
             (if (> (length next-candidate) (length last-longest))
                 (iter (cdr l) next-candidate next-candidate)
                 (iter (cdr l) next-candidate last-longest))
           ))
          (else (iter (cdr l) (list (car l)) last-longest))))
  (iter l '() '()))


(longestDescending '(2 1 3 2 1 2))
(longestDescending '(5 3 8 6 4 2 6 7 1)) ;; 8 6 4 2
(longestDescending '(1 2 3 4 5 6)) ; 1


;;; now is 10:23pm, started at 9:30 ?