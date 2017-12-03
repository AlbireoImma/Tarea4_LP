#lang scheme

#|
Funcion: interes
Descripcion: genera la lista con el interes dado durante un plazo dado
Parametros:
valor flotante
tasa flotante
veces flotante
Retorno: Lista con el interes calculado
________________________________________________________
Nota: Este interes es el interes simple como el del ejemplo
------------------------------------------------------------
|#

(define (interes valor tasa veces)
  (define x (list valor))
  (define y valor)
  (do ((i 0 (+ i 1)))
    ((= i veces) x)
    (set! y (+ (* (/ tasa 100.0) y) y))
    (set! x (append x (list y)))
  )
)

(interes 4 12.5 12)
