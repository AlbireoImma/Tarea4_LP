#lang racket

(define (raiz numero pureza)
  (let fun((x numero) (y pureza) (z (/ numero 2.0)))
  (if (zero? y)
      z
      (fun numero (- y 1) (/ (+ z (/ numero z)) 2.0)))))

(raiz 5 100)