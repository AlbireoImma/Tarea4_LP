#lang racket

#|
Funcion: lenght
Descripcion: Obtiene el largo de la lista
Parametros:
lista lista
Retorno: Un entero con el largo de la lista
|#

(define (lenght lista)
  (if (null? lista)
      0
      (+ 1 (lenght (cdr lista)))
      )
  )

#|
Funcion: operar
Descripcion: Aplica una funcion binaria a elementos de dos listas
Parametros:
fun funcion lambda
lista1 lista
lista2 lista
Retorno: La lista resultante de las operaciones entre los elementos
|#

(define (operar fun)
  (lambda (lista1 lista2)
    (define ret (list))
    (define len (lenght lista1))
    (cond ((null? lista1) ret))
    (do ((i 0 (+ i 1)))
      ((= i len) ret)
      (if (= 1 (car lista1))
          (if (= 1 (car lista2))
              (if (fun #t #t)
                  (set! ret (append ret (list 1)))
                  (set! ret (append ret (list 0))))
              (if (fun #t #f)
                  (set! ret (append ret (list 1)))
                  (set! ret (append ret (list 0)))))
          (if (= 1 (car lista2))
              (if (fun #f #t)
                  (set! ret (append ret (list 1)))
                  (set! ret (append ret (list 0))))
              (if (fun #f #f)
                  (set! ret (append ret (list 1)))
                  (set! ret (append ret (list 0))))))
      (set! lista1 (cdr lista1))
      (set! lista2 (cdr lista2))
          )      
    )
  )


((operar xor) (list 0 1 0 1) (list 1 1 1 0))