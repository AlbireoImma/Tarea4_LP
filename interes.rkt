#lang scheme

(define (interes valor tasa veces)
  (define x (list valor))
  (define y valor)
  (do ((i 0 (+ i 1)))
    ((= i veces) x)
    (set! y (+ (* (/ tasa 100.0) y) y))
    (set! x (append x (list y)))
  )
)
