#lang racket

(define (raiz numero pureza)
  ((lambda (x y)
     (if (= y 0)
         (/ numero 2.0)
         (/ (+ (raiz numero (- y 1)) (/ numero (raiz numero (- y 1)))) 2)))numero pureza)) 

(raiz 5 3)