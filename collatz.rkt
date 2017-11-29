#lang racket

(define (colltz n max)
  (cond ((= n 1) max)
        ((even? n) (if (> max (/ n 2))
                       (colltz (/ n 2) max)
                       (colltz (/ n 2) (/ n 2))))
        (else (if (> max (+ (* n 3) 1))
                  (colltz (+ (* n 3) 1) max)
                  (colltz (+ (* n 3) 1) (+ (* n 3) 1))))
        )
  )


(define (collatz lista)
  (let ((aux (list)))
    ((lambda (ls ls2)
    (if (null? ls)
        ls2
        (append (list (colltz (car ls) (car ls))) (collatz (cdr ls))))
         )lista aux)))

(collatz (list 1 2 3 15))
