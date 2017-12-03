#lang racket

#|
Funcion: raiz
Descripcion: Calcula una aproximacion de raiz cuadrada para un numero
Parametros:
numero entero
pureza entero
Retorno: Retorna la aproximacion segun la pureza requerida
|#

(define (raiz numero pureza)
  (let fun((x numero) (y pureza) (z (/ numero 2.0)))
  (if (zero? y)
      z
      (fun numero (- y 1) (/ (+ z (/ numero z)) 2.0)))))

(raiz 5 1)
(raiz 5 2)
(raiz 5 3)